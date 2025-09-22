
fncFitModel <- function(
    response_var = "obs.stream_temp_daily_mean",
    linear_indices = 1:15,
    dataset = freeflow_data,
    method = "REML",
    ...){

  smooths <-
    "ti(cov.antec_air_temp, k = 6) +
   ti(cov.air_temp_ws, k = 3) +
   ti(cov.daylength_hours, k = 4) +
   ti(cov.std_mean_flow, k = 4) +
   ti(cov.SWE_ws, k = 6) +
   ti(cov.air_temp_ws, cov.antec_air_temp, k = c(3, 6)) +
   ti(cov.daylength_hours, cov.antec_air_temp, k = c(4, 6)) +
   ti(cov.std_mean_flow, cov.antec_air_temp, k = c(4, 6)) +
   ti(cov.SWE_ws, cov.air_temp_ws, k = c(6, 6))"

  linear_primary <- c(
    "cov.lat_v",
    "cov.elev_mean_smo",
    "cov.area_km2_ws_log",
    "cov.BFI_cat",
    "cov.elev_diff",
    "cov.slope",
    "cov.pct_ow_ws",
    "cov.pct_wet_all_ws",
    "cov.pct_ice_ws",
    "cov.pct_for_all_cat_rip100m",
    "cov.canopy_line",
    "cov.pct_urb_all_ws",
    "cov.pct_extru_vol_ws",
    "cov.precip_cat",
    "cov.air_temp_range_cat")

  linear_other <-
    "cov.SWE_1Apr : tim.doy +
    cov.elev_mean_smo : cov.area_km2_ws_log"


  linear_subset <- paste(linear_primary[linear_indices], collapse = "+")

  model_formula <- sprintf(
    "%s ~ %s + %s + %s + %s", # general formula
    response_var, # which response var
    smooths, # fixed terms for smooths
    linear_subset, # subset of linear vars
    sprintf("cov.antec_air_temp:(%s)", linear_subset), # linear vars to interact
    linear_other) |>
    as.formula()


  mgcv::gam(model_formula, data = dataset, method = method, ...)
}
