# Make stream temperature predictions for all reaches

# mean or max
mm <- "mean"

# basin
basin <- "Wenatchee"

# Directories
huc_path <- "data/hucs"
models_path <- "data/results/full"
dir.create("data/predictions")
prediction_path <- "data/predictions"

# Load packages
library(dplyr)
library(mgcv)
library(sf)

# LOAD FITTED MODELS ----
load(paste0(models_path, "/fitted_model_", mm, ".RData"))

#Make model more efficient
strip_glm = function(cm) {
  cm$y = c()
  cm$model = c()
  cm$residuals = c()
  cm$fitted.values = c()
  cm$effects = c()
  cm$qr$qr = c()
  cm$linear.predictors = c()
  cm$weights = c()
  cm$prior.weights = c()
  cm$data = c()
  cm$family$variance = c()
  cm$family$dev.resids = c()
  cm$family$aic = c()
  cm$family$validmu = c()
  cm$family$simulate = c()
  attr(cm$terms,".Environment") = c()
  attr(cm$formula,".Environment") = c()
  return(cm)
}
if(mm == "mean") stream_temp_model <- strip_glm(st_model_mean)
if(mm == "max") stream_temp_model <- strip_glm(st_model_max)
rm(strip_glm)

# load covariates for prediction
covs <- fst::read.fst(path = paste0("data/covariates_", mm, "_", basin, ".fst"))

preds <- predict(stream_temp_model, newdata = covs) # this takes a while

results <- covs |>
  mutate(prd.stream_temp = ifelse(preds < 0, 0, preds)) |>
  tidytable::select(
    "lookup", "COMID", "tim.date", "cov.antec_air_temp",
    "cov.std_mean_flow", "prd.stream_temp")

data.table::fwrite(results, file = paste0("data/predictions/st_pred_", basin, ".csv"))


# EXTRA ----
# Collate across 3 Upper Columbia watersheds (after running model for each)
e <- data.table::fread("data/predictions/st_pred_Entiat.csv")
m <- data.table::fread("data/predictions/st_pred_Methow.csv")
w <- data.table::fread("data/predictions/st_pred_Wenatchee.csv")
results <- rbind(e, m, w)
summary(results)
sum(is.na(results$prd.stream_temp)) / nrow(results) # 1.25% NAs
nrow(results[results$prd.stream_temp > 20,]) / nrow(results) * 100 # 0.45% over 20 C (lower Entiat HUC contains Columbia R etc.)
quantile(results$prd.stream_temp[results$prd.stream_temp > 20], probs = seq(0,1,0.1), na.rm = T)
nrow(results[results$prd.stream_temp > 35,]) / nrow(results) * 100 # 0.045% over 35 C (lower Entiat HUC contains Columbia R etc.)
quantile(results$prd.stream_temp[results$prd.stream_temp > 35], probs = seq(0,1,0.1), na.rm = T)
data.table::fwrite(results, "data/predictions/st_pred_UC.csv")

# Map some results
# load shapefile and xwalk table
xwalk <- read.csv("data/xwalk/COMID_HARP_xwalk.csv") |>
  dplyr::distinct() |>
  filter(NHD_COMID != 24386871) # remove second French Creek reach

flowlines <- st_read("data/shp/smooth_flowlines_v2_merged.shp") |>
  left_join(xwalk, by = "ReachName") |>
  transmute(ReachName, COMID = NHD_COMID)

monthly_means <- results |>
  tidytable::mutate(month = lubridate::month(tim.date)) |>
  tidytable::summarize(
    mean_ST = mean(prd.stream_temp, na.rm = T),
    .by = c(COMID, month))

aug_mean <- tidytable::filter(monthly_means, month == 8)
may_mean <- tidytable::filter(monthly_means, month == 5)

fl_mean_aug <- flowlines |>
  left_join(aug_mean, by = join_by(COMID))

fl_mean_may <- flowlines |>
  left_join(may_mean, by = join_by(COMID))

plot(fl_mean_may["mean_ST"])
plot(fl_mean_aug["mean_ST"])

st_write(fl_mean_aug, "data/shp/fl_mean_aug.shp", layer_options="SHPT=ARC")
st_write(fl_mean_may, "data/shp/fl_mean_may.shp", layer_options="SHPT=ARC")

s <- st_read("data/shp/fl_mean_aug.shp")
plot(s["mean_ST"])

plot(s$mean_ST, fl_mean_aug$mean_ST)
abline(a = 0, b = 1, col = 2)
