# Calculate a delta stream temperature for future periods
library(data.table)
library(dplyr)


# Load predictions from the localized model
st <- data.table::fread("data/st_pred_Wenatchee.csv")
st$year <- lubridate::year(st$tim.date)
st$doy <- lubridate::yday(st$tim.date)
st <- st[!is.na(st$prd.stream_temp),]
st <- st[,c("COMID", "tim.date", "year", "doy", "prd.stream_temp")]

# remove unrealistically high predictions
nrow(st[st$prd.stream_temp >= 30,]) / nrow(st) * 100
hist(st$prd.stream_temp)
idx <- which(st$prd.stream_temp >=30)
st <- st[-idx,]

# Get mean by day of year and reach
meanst <- st %>% 
  group_by(doy, COMID) %>%
  reframe("mean_prd.stream_temp" = mean(prd.stream_temp, na.rm = T))
p10st <- st %>% 
  group_by(doy, COMID) %>%
  reframe("p10_prd.stream_temp" = quantile(prd.stream_temp, probs = 0.1, na.rm = T))
p90st <- st %>% 
  group_by(doy, COMID) %>%
  reframe("p90_prd.stream_temp" = quantile(prd.stream_temp, probs = 0.9, na.rm = T))
retro_st <- left_join(meanst, p10st, by = c("doy", "COMID"))
retro_st <- left_join(retro_st, p90st, by = c("doy", "COMID"))
cid <- 23082478 #23080508
plot(retro_st$doy[retro_st$COMID == cid], retro_st$mean_prd.stream_temp[retro_st$COMID == cid], type = 'l', lwd = 3, ylim = c(0,25), las = 1)
lines(retro_st$doy[retro_st$COMID == cid], retro_st$p10_prd.stream_temp[retro_st$COMID == cid])
lines(retro_st$doy[retro_st$COMID == cid], retro_st$p90_prd.stream_temp[retro_st$COMID == cid])
abline(h = c(0,5,10,15,20), lty = 3)


# Get future scenario data
setwd("D://st-cc/nc_preds")
fls <- dir(getwd())
fl <- fls[grep(".csv", fls)]
basin <- 17020011
fl <- fl[grep(basin, fl)]
climate_scenarios <- c("CanESM2", "CCSM4", "CNRM-CM5", "CSIRO-Mk3-6-0", "GDFL-ESM2M", "HadGEM2-CC", "HadGEM2-ES", "inmcm4", "IPSL-CM5A-MR", "MIROC")
climate_scenarios <- gsub("-", "_", climate_scenarios)
years <- 1990:2099

hucdata <- NULL
for(huc10 in 1:length(fl)){
  
  #Prepare data table  ----
  huc10_data <- data.table::fread(fl[huc10])
  huc10_data$year <- lubridate::year(huc10_data$date)
  huc10_data$doy <- lubridate::yday(huc10_data$date)
  huc10_data$month <- lubridate::month(huc10_data$date)
  cids <- sort(unique(huc10_data$COMID))

  cols <- c("date", "year", "month", "doy", "COMID", climate_scenarios)
  f <- huc10_data[,..cols]
  f <- subset(f, month == 8)
  f <- subset(f, year >= 2020) # exclude older data we have no need for
  f <- subset(f, !is.na(MIROC)) # exclude rows with no predictions
  
  hucdata <- rbind(hucdata, f)
}
# mean across GCMs
hucdata$cmb_mean <- apply(hucdata[, 6:15], 1, sum, na.rm = T) / 10
hucdata$cmb_p10 <- apply(hucdata[, 6:15], 1, quantile, probs = 0.1, na.rm = T)
hucdata$cmb_p90 <- apply(hucdata[, 6:15], 1, quantile, probs = 0.9, na.rm = T)

cid <- 23082478 #23080508
plot(hucdata$doy[hucdata$COMID == cid], hucdata$cmb_mean[hucdata$COMID == cid], type = 'l', lwd = 3, ylim = c(15,25), las = 1)
lines(hucdata$doy[hucdata$COMID == cid], hucdata$cmb_p10[hucdata$COMID == cid], col = 4)
lines(hucdata$doy[hucdata$COMID == cid], hucdata$cmb_p90[hucdata$COMID == cid], col = 2)
lines(hucdata$doy[hucdata$COMID == cid], hucdata$cmb_mean[hucdata$COMID == cid])
abline(h = c(0,5,10,15,20), lty = 3)

# remove unrealistically high predictions
nrow(hucdata[hucdata$cmb_mean >= 30,]) / nrow(hucdata) * 100
idx <- which(hucdata$cmb_mean >= 30)  
hucdata <- hucdata[-idx,]
  

  
# Subtract localized average from mean future temps to retain time series

retro_aug <- retro_st[retro_st$doy >= min(hucdata$doy) & retro_st$doy <= max(hucdata$doy),]

  
  
  
data.table::fwrite(out, paste0("../../UC_PSM/ST_future_", fl[huc10]))
  





# Figure 6A
st <- data.table::fread("data/ST_max_17020011.csv")

for(i in 1:4){
  if(i == 1) {cid <- 23080878; nm <- "White River"} #white
  if(i == 2) {cid <- 23080724; nm <- "Chiwawa River"} #chiwawa
  if(i == 3) {cid <- 23081086; nm <- "Nason Creek"} #nason
  if(i == 4) {cid <- 23080840; nm <- "Wenatchee River"} #wenlower
  
  st_i <- st[st$COMID %in% cid, c("doy", "prd.stream_temp", "clo", "cmn", "chi", "nlo", "nmn", "nhi", "flo", "fmn", "fhi")]
  
  st_medians <- data.frame("doy" = 1:366)
  for(var in c("clo", "cmn", "chi", "nlo", "nmn", "nhi", "flo", "fmn", "fhi", "prd.stream_temp")){
    var_sym <- sym(var)
    result <- st_i %>% 
      group_by(doy) %>%
      reframe(median(!!var_sym, na.rm = T))
    colnames(result)[ncol(result)] <- var
    st_medians <- cbind(st_medians, result[,2])
  }
    
  colrs <- c("#FF990080", "#994F0080", "#0C7BDC80")
  ylms <- c(0, 23)
  
  png(paste0("plots/COMID_", cid, ".png"), width = 6, height = 5, units = "in", res = 300)
  plot(st_medians$doy, st_medians$fmn, cex = .3, type = "n",
       main = paste0(nm, " (COMID ", cid, ")"), las = 1, ylab = "Stream temperature (C)", xlab = "Date", ylim = ylms)
  polygon(c(st_medians$doy, rev(st_medians$doy)), c(st_medians$flo, rev(st_medians$fhi)), col = colrs[1], border = NA)
  polygon(c(st_medians$doy, rev(st_medians$doy)), c(st_medians$nlo, rev(st_medians$nhi)), col = colrs[2], border = NA)
  polygon(c(st_medians$doy, rev(st_medians$doy)), c(st_medians$clo, rev(st_medians$chi)), col = colrs[3], border = NA)
  lines(st_medians$doy, st_medians$prd.stream_temp, cex = .3, lwd = 2, col = "black")
  abline(h = seq(0,20,5), lty = 3, col = "gray40")
  abline(v = pretty(st_i$doy), lty = 3, col = "gray40")
  if(i == 1) {legend("topleft", legend = c("far future", "near future", "current", "predicted"), col = c(colrs, "black"), lwd = 2)}
  dev.off()  
}
  
