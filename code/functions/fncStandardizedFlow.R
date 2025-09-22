# Function predicts standardized flow

# param1 & param2 are from a previously-established relationship (Siegel et al. 2021)
# extreme log outlier values beyond which to exclude

fncStandardizedFlow <- function(the_data, param1 = -4.1, param2 = 0.93, outliers_exclude = -5){

  flow_means <- read.csv("data/nwm/summary_logmean_1990-2022.csv")

  the_data <- tidytable::left_join(the_data, flow_means, by = "COMID") #|>
    #tidytable::rename(
    #  cov.Q_med = Q_median,
    #  cov.Q_mean = Q_mean,
    #  cov.Q_logmean = Q_logmean)

  # Predict flow from log drainage area
  the_data$cov.area_mean_flow  <- param1 + param2 * log(the_data$cov.area_km2_ws)

  # Check that there are non-NA data
  if(any(!is.na(the_data$cov.NWM_flow)) == F) {
    the_data$cov.NWM_flow <- 0.00001
    the_data$cov.NWM_flow_log <- log(the_data$cov.NWM_flow)
  }

  # function to exclude outliers
  excl_outliers <- function(x) tidytable::if_else(x < outliers_exclude, outliers_exclude, x)

  the_data |>
    tidytable::mutate( # calculate differences between daily flow and mean flow
      cov.std_mean_flow_DA = excl_outliers(cov.NWM_flow_log - cov.area_mean_flow),
      cov.std_mean_flow = excl_outliers(cov.NWM_flow_log - cov.Q_logmean))

}

