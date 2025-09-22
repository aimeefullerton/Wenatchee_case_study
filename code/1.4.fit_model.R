# Fit a stream temperature model for free-flowing reaches

library(mgcv)
library(dplyr)
options(scipen = 10)

# SETUP ----
# Functions
source("code/functions/fncFitModel.R")
source("code/0_functions/fncPlotConditionalEffects.R")
source("code/0_functions/fncPlotResidsByCovars.R")

# Directories
plot.dir <- "plots"
dir.create(plot.dir, showWarnings = F)
plot_it <- T
dir.create("data/results", showWarnings = F)
models_path <- "data/results/full"
dir.create(models_path, showWarnings = F)
models_path <- "data/results/full"

# LOAD DATA ----
freeflow_data <- fst::read_fst("data/freeflow_data.fst")
freeflow_data_max <- fst::read_fst("data/freeflow_data_max.fst")

# FIT MODEL ----
st_model_mean <- fncFitModel(
  response_var = "obs.stream_temp_daily_mean",
  dataset = freeflow_data,
  linear_indices = c(1:7, 9:12, 14:15),
  method = "REML")
summary(st_model_mean)
sum(st_model_mean$edf)
save(st_model_mean, file = paste0(models_path, "/fitted_model_mean.RData"))

st_model_max <- fncFitModel(
  response_var = "obs.stream_temp_daily_max",
  dataset = freeflow_data_max,
  linear_indices = c(1:5, 7, 9:11, 14:15),
  method = "REML")
summary(st_model_max)
sum(st_model_max$edf)
save(st_model_max, file = paste0(models_path, "/fitted_model_max.RData"))


# PREDICT ----
# means
freeflow_data$model_pred <-  predict(st_model_mean, newdata = freeflow_data)
freeflow_data$model_pred <- ifelse(freeflow_data$model_pred < 0, 0, freeflow_data$model_pred)
freeflow_data$model_resid <- freeflow_data$obs.stream_temp_daily_mean - freeflow_data$model_pred
plot(freeflow_data$obs.stream_temp_daily_mean, freeflow_data$model_pred, pch = "."); abline(0, 1, col = "red")

#max
freeflow_data_max$model_pred <-  predict(st_model_max, newdata = freeflow_data_max)
freeflow_data_max$model_pred <- ifelse(freeflow_data_max$model_pred < 0, 0, freeflow_data_max$model_pred)
freeflow_data_max$model_resid <- freeflow_data_max$obs.stream_temp_daily_max - freeflow_data_max$model_pred
plot(freeflow_data_max$obs.stream_temp_daily_max, freeflow_data_max$model_pred, pch = "."); abline(0, 1, col = "red")

# export
fst::write_fst(freeflow_data, paste0(models_path, "/freeflow_data.fst"), compress = 100)
fst::write_fst(freeflow_data_max, paste0(models_path, "/freeflow_data_max.fst"), compress = 100)


# COMPUTE MAE and RMSE ----
# Means ----
# Create container for holding summary results
results_summary <- matrix(ncol = 3, nrow = 5)
colnames(results_summary) <- c("Full", "Txv", "Sxv")
row.names(results_summary) <- c("RMSE_daily", "MAE_daily", "RMSE_monthly", "MAE_monthly", "N")

# Function for adding MAE and RMSE to results summary
calc_RMSE <- \(x) sqrt(mean(x^2, na.rm = T))
calc_MAE  <- \(x) mean(abs(x), na.rm =T)

results_summary[1,1] <- calc_RMSE(freeflow_data$model_resid)
results_summary[2,1] <- calc_MAE(freeflow_data$model_resid)

# View results subsetted by hydrological regime
freeflow_data |>
  mutate(hydroreg = case_when(
    cov.SWE_mean_year < 20 ~ "rain",
    cov.SWE_mean_year >= 20 & cov.SWE_mean_year < 100 ~ "trans",
    cov.SWE_mean_year >= 100 ~ "snow")) |>
  summarize( # best in snow
    MAE = calc_MAE(model_resid),
    RMSE = calc_RMSE(model_resid),
    .by = hydroreg)
freeflow_data |>
  mutate(hydroreg = case_when(
    cov.SWE_mean_year < 20 ~ "rain",
    cov.SWE_mean_year >= 20 & cov.SWE_mean_year < 100 ~ "trans",
    cov.SWE_mean_year >= 100 ~ "snow")) |>
  summarize( # best in snow
    n(),
    .by = hydroreg)

# Monthly residuals; add to summary file
compare_monthly <- freeflow_data |>
  summarize(
    obs_mean_monthly = mean(obs.stream_temp_daily_mean, na.rm = T),
    pred_mean_monthly = mean(model_pred),
    .by = c(COMID_year_month)) %>%
  mutate(monthly_resid = pred_mean_monthly - obs_mean_monthly)

plot(obs_mean_monthly ~ pred_mean_monthly, data = compare_monthly, pch = "."); abline(0, 1, col = "red")
results_summary[3,1] <- calc_RMSE(compare_monthly$monthly_resid)
results_summary[4,1] <- calc_MAE(compare_monthly$monthly_resid)
results_summary[5,1] <- nrow(freeflow_data)

# Residuals by site
resid_by_site <- freeflow_data |>
  summarize(mean_resid = mean(model_resid, na.rm = T), .by = NorWeST_ID)


# find sites with highest residuals
freeflow_data |>
  filter(abs(model_resid) > 5) |>
  count(COMID) |>
  filter(n > 5) |>
  arrange(desc(n)) |>
  data.table::fwrite(paste0(models_path, "/means_residuals_5_over_5.csv"))

freeflow_data |> filter(COMID == 23208310) |>
  select(COMID, NorWeST_ID, obs.stream_temp_daily_mean, tim.date, model_pred, model_resid) |>
  arrange(tim.date) |> View()

# Max ----

# Create container for holding summary results
results_summary_max <- matrix(ncol = 3, nrow = 5)
colnames(results_summary_max) <- c("Full", "Txv", "Sxv")
row.names(results_summary_max) <- c("RMSE_daily", "MAE_daily", "RMSE_monthly", "MAE_monthly", "N")

results_summary_max[1,1] <- calc_RMSE(freeflow_data_max$model_resid)
results_summary_max[2,1] <- calc_MAE(freeflow_data_max$model_resid)

# View results subsetted by hydrological regime
freeflow_data_max |>
  mutate(hydroreg = case_when(
    cov.SWE_mean_year < 20 ~ "rain",
    cov.SWE_mean_year >= 20 & cov.SWE_mean_year < 100 ~ "trans",
    cov.SWE_mean_year >= 100 ~ "snow")) |>
  summarize( # best in snow
    MAE = calc_MAE(model_resid),
    RMSE = calc_RMSE(model_resid),
    .by = hydroreg)

# Monthly residuals; add to summary file
compare_monthly <- freeflow_data_max |>
  summarize(
    obs_mean_monthly = mean(obs.stream_temp_daily_max, na.rm = T),
    pred_mean_monthly = mean(model_pred),
    .by = c(COMID_year_month)) %>%
  mutate(monthly_resid = pred_mean_monthly - obs_mean_monthly)

plot(obs_mean_monthly ~ pred_mean_monthly, data = compare_monthly, pch = "."); abline(0, 1, col = "red")
results_summary_max[3,1] <- calc_RMSE(compare_monthly$monthly_resid)
results_summary_max[4,1] <- calc_MAE(compare_monthly$monthly_resid)
results_summary_max[5,1] <- nrow(freeflow_data_max)


# find sites with highest residuals
freeflow_data_max |>
  filter(abs(model_resid) > 5) |>
  count(COMID) |>
  filter(n > 5) |>
  arrange(desc(n)) |>
  # View()
  data.table::fwrite(paste0(models_path, "/residuals_maxes_5_over_5.csv"))

freeflow_data_max |> filter(COMID == 23081392) |>
  select(COMID, obs.stream_temp_daily_max, tim.date, model_pred, model_resid) |>
  arrange(tim.date) |> View()

# PLOTS ----

  # Conditional Effects
set.seed(123)
fncPlotConditionalEffects(
  the_data = dplyr::sample_n(freeflow_data, 10000),
  the_model = st_model_mean)

  # Residuals by Covariates
set.seed(123)
fncPlotResidsByCovars(the_data = dplyr::sample_n(freeflow_data, 10000))

# TEMPORAL CROSS-VALIDATION (LOYOCV; Leave One Year Out) ----

# Initialize container dataframe

freeflow_Txv <- NULL
lin_ind <- c(1:7, 9:12, 14:15)
# Loop
for(y in 1993:2021){ #years with response data
  print(y)
  # Separate data into fitting and testing subsets
  freeflow_test <- freeflow_data[freeflow_data$tim.year == y, ]
  freeflow_fit <- freeflow_data[freeflow_data$tim.year != y, ]

  region17_model_Txv <- fncFitModel(
    dataset = freeflow_fit, linear_indices = lin_ind)

  freeflow_test$pred_Txv <- predict(region17_model_Txv, newdata = freeflow_test)
  freeflow_Txv <- rbind(freeflow_Txv, freeflow_test)
}

# Add residuals to results file
freeflow_Txv$pred_Txv  <- ifelse(freeflow_Txv$pred_Txv < 0, 0, freeflow_Txv$pred_Txv)
freeflow_Txv$resid_Txv <- freeflow_Txv$obs.stream_temp_daily_mean - freeflow_Txv$pred_Txv

# Add MAE and RMSPE stats to the results summary table
results_summary[1,2] <- calc_RMSE(freeflow_Txv$resid_Txv)
results_summary[2,2] <- calc_MAE(freeflow_Txv$resid_Txv)

# View results summarized by year and month
tapply(abs(freeflow_Txv$resid_Txv), freeflow_Txv$tim.year, mean, na.rm = T)
tapply(abs(freeflow_Txv$resid_Txv), freeflow_Txv$tim.month, mean, na.rm = T)

# Monthly Txv residuals; add to summary file
freeflow_Txv$COMID_year_month <- paste0(freeflow_Txv$COMID, "_", freeflow_Txv$tim.year, "_", freeflow_Txv$tim.month)
pred_mean_monthly <- as.data.frame.table(tapply(abs(freeflow_Txv$pred_Txv), freeflow_Txv$COMID_year_month, mean, na.rm = T))
obs_mean_monthly <- as.data.frame.table(tapply(abs(freeflow_Txv$obs.stream_temp_daily_mean), freeflow_Txv$COMID_year_month, mean, na.rm = T))
compare_monthly <- merge(obs_mean_monthly, pred_mean_monthly, by = "Var1", all.x = T)
colnames(compare_monthly) <- c("COMID_year_month", "obs_mean_monthly", "pred_mean_monthly")
plot(obs_mean_monthly ~ pred_mean_monthly, data = compare_monthly, pch = "."); abline(0, 1, col = "red")
compare_monthly$resid  <- compare_monthly$obs_mean_monthly - compare_monthly$pred_mean_monthly
results_summary[3,2] <- calc_RMSE(compare_monthly$resid)
results_summary[4,2] <- calc_MAE(compare_monthly$resid)
results_summary[5,2] <- nrow(freeflow_Txv)

fst::write_fst(freeflow_Txv, paste0(models_path, "/freeflow_Txv.fst"), compress = 100)


# EXPORT FIT STATISTICS ----
write.csv(results_summary, paste0(models_path, "/freeflow_data_results_summary.csv"))
write.csv(results_summary_max, paste0(models_path, "/freeflow_data_max_results_summary.csv"))
