# Prepare covariates needed for the localized model

# SETUP ----
# Directories
huc_path <- "PNW_covariates"

# Load packages
library(dplyr)
library(mgcv)

# Functions
source("code/functions/fncStandardizedFlow.R")
source("code/functions/fncHydroRegion.R")
source("code/functions/fncAntecedentPeriod.R")
source("code/functions/fncRoundLags.R")
source("code/functions/fncAntecedentAirTemp.R")

# LOAD DATA ----
COM_HUC <- data.table::fread("data/COMID_to_HUC12.csv")
COM_HUC$Huc6 <- substr(COM_HUC$Huc12, 1, 6)
COM_HUC$Huc10 <- substr(COM_HUC$Huc12, 1, 10)
huclist <- sort(unique(COM_HUC$Huc6))
huc10list <- dir(huc_path); h <- gsub("huc_", "", huc10list); h <- gsub(".fst", "", h)
spatial_data <- data.table::fread("data/spatial_data.csv")

# LOAD FITTED MODELS ----
load("data/antec_air_temp_duration_models.RData") # means
load("data/antec_air_temp_duration_models_max.RData") #max


# combine everything
get_covs <- function(huc10file, meanmax){
  # Load pre-processed covariate data
  huc_data <- fst::read_fst(file.path(huc_path, huc10file), as.data.table = T) |>
    # fix dates
    tidytable::mutate(tim.date = as.Date(paste(tim.year, tim.doy), format = "%Y %j")) |>
    tidytable::left_join(
      spatial_data[,c("COMID", "cov.area_km2_ws", "cov.proportion_dam_influenced")],
      .by = "COMID") |>
    fncStandardizedFlow()

  if(nrow(huc_data) == 0) return(NULL)

  # Split by hydrological region ----
  hyd_reg <- fncHydroRegion(the_data = huc_data)
  huc_data_rain <- hyd_reg[["rain"]]
  huc_data_trans <- hyd_reg[["trans"]]
  huc_data_snow <- hyd_reg[["snow"]]
  rm(hyd_reg)

  # Predict best lag for antecedent air temperature
  if(meanmax == "mean"){
    huc_data_rain$pred_lag <- predict(rain_lag_model, newdata = huc_data_rain)
    huc_data_trans$pred_lag <- predict(trans_lag_model, newdata = huc_data_trans)
    huc_data_snow$pred_lag <- predict(snow_lag_model, newdata = huc_data_snow)
  } else {
    huc_data_rain$pred_lag <- predict(rain_lag_model_max, newdata = huc_data_rain)
    huc_data_trans$pred_lag <- predict(trans_lag_model_max, newdata = huc_data_trans)
    huc_data_snow$pred_lag <- predict(snow_lag_model_max, newdata = huc_data_snow)
  }

  huc_data_rain$pred_lag <- fncRoundLags(the_data = huc_data_rain)
  huc_data_trans$pred_lag <- fncRoundLags(the_data = huc_data_trans)
  huc_data_snow$pred_lag <- fncRoundLags(the_data = huc_data_snow)

  # Recombine hydroregion data subsets
  huc_data <- rbind(huc_data_rain, huc_data_trans, huc_data_snow)

  rm(huc_data_rain, huc_data_trans, huc_data_snow)

  # Predict antecedent air temperature based on optimal window size
  huc_data <- fncAntecedentAirTemp(the_data = huc_data)
  print(sprintf(
    "%s: %s",
    huc10file, sum(is.na(huc_data$cov.antec_air_temp))/(nrow(huc_data))))

  # ADD SPATIAL COVARIATES ----
  tidytable::left_join(huc_data, spatial_data, by = "COMID")
}


basin <- "Wenatchee"
hhh <- 17020011
huc10list <- huc10list[grep(hhh, huc10list)]

all_covs_mean <- purrr::map_dfr(huc10list, \(x)get_covs(x, "mean"))
fst::write.fst(x = all_covs_mean, path = paste0("data/covariates_mean_", basin, ".fst"))

all_covs_max <- purrr::map_dfr(huc10list, \(x)get_covs(x, "max"))
fst::write.fst(x = all_covs_max, path = paste0("data/covariates_max_", basin, ".fst"))
