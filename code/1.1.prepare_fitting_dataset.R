# Create dataset of reach/day combinations used for fitting models by sequentially merging in components of previously prepared covariates

# Load packages
library(dplyr)
library(mgcv)
library(tidytable)

huc_path <- "PNW_covariates"

# LOAD OBSERVATIONS ----
nw_data <- data.table::fread("data/NorWeST_obs.csv")

stdat <- data.table::fread("data/WenTemps3Jun25.csv")
stdat <- stdat[!is.na(stdat$COMID),]
stdat <- stdat[, c("Date", "COMID", "AvgDailyTemp", "MinDailyTemp", "MaxDailyTemp")]
colnames(stdat) <- c("tim.date", "COMID", "obs.stream_temp_daily_mean", "obs.stream_temp_daily_min", "obs.stream_temp_daily_max")
stdat$tim.year <- lubridate::year(stdat$tim.date)
stdat$tim.doy <- lubridate::yday(stdat$tim.date)
stdat$lookup <- paste0(stdat$COMID, "_", stdat$tim.year, "_", stdat$tim.doy)

# Merge nw_data and new data
obs_data <- rbind.data.frame(nw_data[, c("tim.date", "COMID",
                                         "obs.stream_temp_daily_mean", "obs.stream_temp_daily_min", "obs.stream_temp_daily_max",
                                         "tim.year", "tim.doy", "lookup")], stdat)
obs_data <- unique(obs_data)
obs_data <- obs_data[!obs_data$COMID %in% c(24120842, 23207982, 24125519),] # remove bad sensors

target_hucs <- c(
 # "17030001", #(upper yakima)
  "17020011", # wenatchee)
  "17020010", #(entiat+)
  "17020008" #(methow)
  #"17020006", #(okanogan)
  #"17020007"  #(upper okanogan)
)

xwalk <- read.csv("data/COMID_to_HUC12.csv")
xwalk$HUC8 <- substr(xwalk$Huc12, 1,8)
cids <- xwalk$COMID[xwalk$HUC8 %in% target_hucs]

obs_data <- obs_data[obs_data$COMID %in% cids,]
rm(nw_data, stdat)


# Add covariates
huc10list <- dir(huc_path); huc10list <- gsub("huc_", "", huc10list); huc10list <- gsub(".fst", "", huc10list)

h10s <- NULL
for(huc in target_hucs){
  h10s <- c(h10s, huc10list[grep(huc, huc10list)])
}

# Load pre-processed covariate data
covars <- NULL
for(h in h10s){
  huc_data <- fst::read_fst(paste0(huc_path, "/huc_", h, ".fst"), as.data.table = T)
  covars <- rbind(covars, huc_data)
}

# Create new flow summary (called later by fncStandardizedFlow)
  # Check that there are non-NA data
  if(any(!is.na(covars$cov.NWM_flow)) == F) {
    covars$cov.NWM_flow <- 0.00001
    covars$cov.NWM_flow_log <- log(covars$cov.NWM_flow)
  }
q1 <- covars %>% group_by(COMID) %>% summarise(cov.Q_mean = mean(cov.NWM_flow, na.rm = T), cov.Q_median = median(cov.NWM_flow, na.rm = T))
q1$cov.Q_logmean <- log(q1$cov.Q_mean)
write.csv(q1, "data/summary_logmean_1990-2022.csv")

fst::write_fst(covars, "data/covars.fst", compress = 80)

# spatial dataset
spatial_data <- data.table::fread("data/spatial_data.csv")

# MERGE
fitting_data <- merge(obs_data[, c("lookup", "obs.stream_temp_daily_mean", "obs.stream_temp_daily_min", "obs.stream_temp_daily_max")], covars, by = "lookup", all.x = T)
fitting_data <- merge(fitting_data, spatial_data, by = "COMID", all.x = T)

# EXPORT ----
fst::write_fst(fitting_data, "data/fitting_data.fst", compress = 80)
rm(covars, obs_data, spatial_data)
