# Produce antecedent air temperature prediction models and freeflowing dataset

# Load packages
library(dplyr)
library(mgcv)
library(tidytable)


# Functions
source("code/functions/fncFreeFlowing.R")
source("code/functions/fncStandardizedFlow.R")
source("code/functions/fncHydroRegion.R")
source("code/functions/fncAntecedentPeriod.R")
source("code/functions/fncAntecedentPeriodMax.R")
source("code/functions/fncRoundLags.R")
source("code/functions/fncAntecedentAirTemp.R")
source("code/functions/fncPlotAntecedentLag.R")

plot.dir <- "plots"


fitting_data <- fst::read_fst("data/fitting_data.fst")

# Filter to Free-flowing
freeflow_data <- fitting_data |>
  tidytable::filter(cov.area_km2_ws < 20000)|> # remove large watersheds
  fncFreeFlowing(PDI = 0.25) # only free-flowing reaches

# ADD STANDARDIZED FLOW ----
freeflow_data <- fncStandardizedFlow(the_data = freeflow_data)

# SPLIT INTO HYDROLOGICAL REGIONS ----
hyd_reg <- fncHydroRegion(the_data = freeflow_data)
rm(freeflow_data, fitting_data)

# Evaluate: summarize mean across years for each class
# count is total n observations
# n_COMIDS is number of unique reaches, by year

summarize_hydro <- function(hr){
  hr |>
    tidytable::summarize(
      count = n(),
      n_COMIDs = length(unique(COMID)),
      SWE = mean(cov.SWE_mean_year),
      SWE_sd = sd(cov.SWE_mean_year),
      .by = tim.year)}

r <- summarize_hydro(hyd_reg[["rain"]])
t <- summarize_hydro(hyd_reg[["trans"]])
s <- summarize_hydro(hyd_reg[["snow"]])

(mean(r$n_COMIDs)); (range(r$n_COMIDs))
(mean(t$n_COMIDs)); (range(t$n_COMIDs))
(mean(s$n_COMIDs)); (range(s$n_COMIDs))


# PREDICT ANTECEDENT AIR TEMPERATURE ----
calc_antec <- function(meanmax, flows = hyd_reg){
  
  if(meanmax == "mean"){
    print("Fitting means")
    # Get optimal antecedent period duration for daily means
    rain_out  <- fncAntecedentPeriod(the_data = flows[["rain"]])
    trans_out <- fncAntecedentPeriod(the_data = flows[["trans"]])
    snow_out  <- fncAntecedentPeriod(the_data = flows[["snow"]])
  } else{
    print("Fitting max")
    # and for daily max
    rain_out  <- fncAntecedentPeriodMax(the_data = flows[["rain"]])
    trans_out <- fncAntecedentPeriodMax(the_data = flows[["trans"]])
    snow_out  <- fncAntecedentPeriodMax(the_data = flows[["snow"]])
  }
  
  # Add predicted optimal antecedent duration
  flows[["rain"]]$pred_lag  <- rain_out[[1]]
  flows[["trans"]]$pred_lag <- trans_out[[1]]
  flows[["snow"]]$pred_lag  <- snow_out[[1]]
  
  # Save optimal antecedent duration models
  "rain_lag_model" <- rain_out[[2]]
  "trans_lag_model" <- trans_out[[2]]
  "snow_lag_model" <- snow_out[[2]]
  
  # Correlation categories
  "rain_corr_categs" <- rain_out[[3]]
  "trans_corr_categs" <- trans_out[[3]]
  "snow_corr_categs" <- snow_out[[3]]
  
  # Predict antecedent air temperature based on optimal window durations for
  #rain, trans, and snow subsets
  freeflow_data_rain  <- fncAntecedentAirTemp(the_data = flows[["rain"]])
  freeflow_data_trans <- fncAntecedentAirTemp(the_data = flows[["trans"]])
  freeflow_data_snow  <- fncAntecedentAirTemp(the_data = flows[["snow"]])
  
  # Plots to evaluate lags
  fncPlotAntecedentLag(
    rain_lag_model, trans_lag_model, snow_lag_model,
    rain_corr_categs, trans_corr_categs, snow_corr_categs,
    freeflow_data_rain, freeflow_data_trans, freeflow_data_snow, meanmax)
  
  # Recombine hydroregion data subsets
  freeflow_data <- rbind(
    freeflow_data_rain, freeflow_data_trans, freeflow_data_snow) |>
    mutate( # and add time info
      tim.date = as.Date(paste(tim.doy, tim.year), format = "%j %Y"),
      tim.month = lubridate::month(tim.date),
      COMID_year_month = paste0(COMID, "_", tim.year, "_", tim.month))
  
  # EXPORT DATA WITH ANTECEDENT AIR INFO & LAG MODEL ----
  if(meanmax == "mean"){
    fst::write_fst(
      freeflow_data, "data/freeflow_data.fst", compress = 80)
    save( # save antecedent models
      rain_lag_model, trans_lag_model, snow_lag_model,
      file = "data/antec_air_temp_duration_models.RData")
  } else{ # save max fitting data separately
    fst::write_fst(
      freeflow_data,
      "data/freeflow_data_max.fst", compress = 80)
    rain_lag_model_max <- rain_lag_model
    trans_lag_model_max <- trans_lag_model
    snow_lag_model_max <- snow_lag_model
    save(
      rain_lag_model_max, trans_lag_model_max, snow_lag_model_max,
      file = "data/antec_air_temp_duration_models_max.RData")
  }
}

calc_antec("mean")
calc_antec("max")



