# Calculate a delta stream temperature for future periods
library(dplyr)
library(data.table)
library(ggplot2)
library(sf)

# Aggregate function
fncAggBy <- function(dat, filter_month = 1:12, reach = COMID, timestep = doy, stat = "mean", p = 0.95){
    result <- dat |>
      mutate(
        month = as.character(lubridate::month(Date)),
        year = as.character(lubridate::year(Date)),
        doy = as.character(lubridate::yday(Date))) |>
      group_by({{reach}}, {{timestep}}) |>
      filter(month %in% filter_month)
  if(stat == "mean"){
      result <- result |>
        summarise(across(
          where(is.numeric),
          ~ mean(.x, na.rm = T),
          .names = "mean_{.col}"),
          .groups = "drop") 
  } else { # quantile
      result <- result |>
        summarise(across(
          where(is.numeric),
          ~ quantile(.x, probs = p, na.rm = T),
          .names = paste0("q", substr(p,3,4), "_{.col}")),
          .groups = "drop")
  }
  return(result)
}
 

# Prepare and aggregate empirical data ----
emp <- data.table::fread("data/WenTemps3Jun25.csv")
emp <- emp[!is.na(emp$COMID), c("Date", "COMID", "AvgDailyTemp")]

# Aggregate by day of year and reach
emp_agg <- cbind(fncAggBy(dat = emp, stat = "q", p = 0.05), fncAggBy(dat = emp, stat = "mean")[,3], 
                 fncAggBy(dat = emp, stat = "q", p = 0.95)[,3])
emp_agg$doy <- as.numeric(emp_agg$doy); emp_agg <- emp_agg[order(emp_agg$COMID, emp_agg$doy),]
data.table::fwrite(emp_agg, "data/emp_agg.csv")


# Prepare and aggregate retrospective period (1990-2020) predictions from the localized model ----
st <- data.table::fread("data/st_pred_Wenatchee.csv")
st <- st[!is.na(st$prd.stream_temp),]
st <- st[,c("COMID", "tim.date", "prd.stream_temp")]
colnames(st)[2] <- "Date"

  # evaluate and remove unrealistically high predictions
  nrow(st[st$prd.stream_temp >= 30,]) / nrow(st) * 100
  hist(st$prd.stream_temp)
  idx <- which(st$prd.stream_temp >=30)
  foo <- st[idx,]
  foo$doy <- lubridate::yday(foo$Date)
  plot(foo$doy, foo$prd.stream_temp)
  hist(lubridate::year(foo$Date))
  cids <- sort(unique(foo$COMID))
  streams <- read_sf("data/shp/upper_columbia_streams_nhd2.shp")
  spawn_reaches <- read.csv("data/Wenatchee_spawn_reaches.csv")
  streams <- dplyr::left_join(streams, spawn_reaches, join_by(COMID))
  ss <- streams[streams$COMID %in% cids,] #none are spawning reaches - many are unnamed
  # likely safe to remove these anomalous reaches
  st <- st[-idx,]
  rm(cids, idx, foo, ss)

# Aggregate retrospective period localized model predictions by day of year and reach
retro_agg <- cbind(fncAggBy(dat = st, stat = "q", p = 0.05), fncAggBy(dat = st, stat = "mean")[,3], 
                 fncAggBy(dat = st, stat = "q", p = 0.95)[,3])
retro_agg$doy <- as.numeric(retro_agg$doy); retro_agg <- retro_agg[order(retro_agg$COMID, retro_agg$doy),]
data.table::fwrite(retro_agg, "data/retro_agg.csv")


# Prepare and aggregate future period predictions ----
setwd("D://st-cc/nc_preds")
fls <- dir(getwd())
fl <- fls[grep(".csv", fls)]
basin <- 17020011
fl <- fl[grep(basin, fl)]
climate_scenarios <- c("CanESM2", "CCSM4", "CNRM-CM5", "CSIRO-Mk3-6-0", "GDFL-ESM2M", "HadGEM2-CC", "HadGEM2-ES", "inmcm4", "IPSL-CM5A-MR", "MIROC")
climate_scenarios <- gsub("-", "_", climate_scenarios)
years <- 1990:2099

# Collate single data table
hucdata <- NULL
for(huc10 in 1:length(fl)){
  huc10_data <- data.table::fread(fl[huc10])
  cids <- sort(unique(huc10_data$COMID))
  cols <- c("date", "COMID", climate_scenarios)
  f <- huc10_data[,..cols]
  f <- subset(f, lubridate::year(f$date) %in% years) # exclude older data we have no need for
  hucdata <- rbind(hucdata, f)
}
colnames(hucdata)[1] <- "Date"

# Remove unrealistically high predictions (and NA values)
filtered <- hucdata %>%
  filter(if_all(3:12, ~ .x < 30))
setwd("~/GitHub/Wenatchee_case_study")
data.table::fwrite(filtered, "data/future_wide.csv")

# Pivot longer (COMID, date, GCM, STvalue)
fut_long <- tidyr::pivot_longer(data = filtered, 
                    cols = 'CanESM2':'MIROC',
                    names_to = "GCM",
                    values_to = "STvalue")
data.table::fwrite(fut_long, "data/future_long.csv")


# Aggregate ensemble GCM estimates by day of year and reach for 1990-2020 period
period_data <- fut_long[lubridate::year(fut_long$Date) > 1989 & lubridate::year(fut_long$Date) < 2021, c("Date", "COMID", "STvalue")]
agg9020 <- cbind(fncAggBy(dat = period_data, stat = "q", p = 0.05), fncAggBy(dat = period_data, stat = "mean")[,3], 
                   fncAggBy(dat = period_data, stat = "q", p = 0.95)[,3])
agg9020$doy <- as.numeric(agg9020$doy); agg9020 <- agg9020[order(agg9020$COMID, agg9020$doy),]

# Combine localized model, future model for 1990-2020 period, and empirical data
agg9020 <- dplyr::left_join(retro_agg, agg9020, by = c("COMID", "doy"))
agg9020 <- as.data.frame(dplyr::left_join(agg9020, emp_agg, by = c("COMID", "doy")))
data.table::fwrite(agg9020, "data/agg9020.csv")

# examine for a day in Aug
foo <- agg9020[agg9020$doy %in% 220 & !is.na(agg9020$mean_AvgDailyTemp),]
summary(lm(foo$mean_STvalue ~ foo$mean_prd.stream_temp))
summary(lm(foo$mean_AvgDailyTemp ~ foo$mean_prd.stream_temp))
plot(foo$mean_prd.stream_temp, foo$mean_STvalue, ylim = c(5,25), xlim = c(5,25))
points(foo$mean_prd.stream_temp, foo$mean_AvgDailyTemp, col = 2)
abline(a = 4.76870, b = 0.69735)
abline(a = -0.46426, b = 1.06832, col = 2)
abline(a = 0, b = 1, lty = 3, col = "gray50") #1:1 line
summary(foo)
  # Conclusion: the future stream temp model definitely needs to be adjusted (black dots/line on graph) 
  # The localized model matches the empirical data better in terms of the slope (red dots/line on graph); see evaluation above ~ line 71

# Create adjustment
agg9020$pAdj <- (agg9020$mean_prd.stream_temp + 1) / (agg9020$mean_STvalue + 1)
agg9020$pAdj05 <- (agg9020$q05_prd.stream_temp + 1) / (agg9020$q05_STvalue + 1)
agg9020$pAdj95 <- (agg9020$q95_prd.stream_temp + 1) / (agg9020$q95_STvalue + 1)
data.table::fwrite(agg9020, "data/agg9020.csv")

hist(agg9020$pAdj, breaks = 40)
hist(agg9020$pAdj05, breaks = 40, col = rgb(0,0,1,0.5), add = T)
hist(agg9020$pAdj95, breaks = 40, col = rgb(1,0,0,0.5), add = T)


# Adjust future predictions based on 1990-2020 period
fut_pds <- fut_long[lubridate::year(fut_long$Date) > 2020,]
fut_pds$doy <- lubridate::yday(fut_pds$Date)
fut_pds <- dplyr::left_join(fut_pds, agg9020[, c("COMID", "doy", "pAdj", "pAdj05", "pAdj95")], by = c("COMID", "doy"))

fut_pds$STadj <- fut_pds$STvalue * fut_pds$pAdj
fut_pds$STadj05 <- fut_pds$STvalue * fut_pds$pAdj05
fut_pds$STadj95 <- fut_pds$STvalue * fut_pds$pAdj95
data.table::fwrite(fut_pds, "data/future_adjusted.csv")

hist(fut_pds$STadj, breaks = 40)
hist(fut_pds$STadj05, breaks = 40, col = rgb(0,0,1,0.5), add = T)
hist(fut_pds$STadj95, breaks = 40, col = rgb(1,0,0,0.5), add = T)


fncAggPeriod <- function(year1, year2){
  period_data <- fut_pds[lubridate::year(fut_pds$Date) >= year1 & lubridate::year(fut_pds$Date) <= year2, c("Date", "COMID", "STadj", "STadj05", "STadj95")]
  dat <- cbind(fncAggBy(dat = period_data[,c("Date", "COMID", "STadj05")], stat = "q", p = 0.05), 
               fncAggBy(dat = period_data[,c("Date", "COMID", "STadj")], stat = "mean")[,3], 
               fncAggBy(dat = period_data[,c("Date", "COMID", "STadj95")], stat = "q", p = 0.95)[,3])
  dat$doy <- as.numeric(dat$doy); dat <- dat[order(dat$COMID, dat$doy),]
  data.table::fwrite(dat, paste0("data/agg", year1, "s.csv"))
  
  return(dat)
}

# Aggregate by day of year and reach for different decades
agg2020s <- fncAggPeriod(2020, 2029)
agg2040s <- fncAggPeriod(2040, 2049)
agg2060s <- fncAggPeriod(2060, 2069)
agg2080s <- fncAggPeriod(2080, 2089)





# Figure 6A ----
agg9020 <- as.data.frame(data.table::fread("data/agg9020.csv"))
agg2020s <- as.data.frame(data.table::fread("data/agg2020s.csv"))
agg2040s <- as.data.frame(data.table::fread("data/agg2040s.csv"))
agg2060s <- as.data.frame(data.table::fread("data/agg2060s.csv"))
agg2080s <- as.data.frame(data.table::fread("data/agg2080s.csv"))
emp_agg <- as.data.frame(data.table::fread("data/emp_agg.csv"))
fut_pds <- as.data.frame(data.table::fread("data/future_adjusted.csv"))

for(i in 1:5){
  if(i == 1) {cids <- spawn_reaches$COMID[spawn_reaches$spawn_reach %in% "white"]; nm <- "White River"} 
  if(i == 2) {cids <- spawn_reaches$COMID[spawn_reaches$spawn_reach %in% "chiwawa"]; nm <- "Chiwawa River"} 
  if(i == 3) {cids <- spawn_reaches$COMID[spawn_reaches$spawn_reach %in% "nason"]; nm <- "Nason Creek"}
  if(i == 4) {cids <- spawn_reaches$COMID[spawn_reaches$spawn_reach %in% "wenlower"]; nm <- "Wenatchee River"}
  if(i == 5) {cids <- spawn_reaches$COMID[spawn_reaches$spawn_reach %in% "wenmigr"]; nm <- "Migration Corridor"}

  # Aggregate over the cids selected
  dd <- agg9020[agg9020$COMID %in% cids,]
  dd1 <- dd %>% group_by(doy) %>% reframe(mean(mean_prd.stream_temp, na.rm = T))
  dd2 <- dd %>% group_by(doy) %>% reframe(mean(q05_prd.stream_temp, na.rm = T))
  dd3 <- dd %>% group_by(doy) %>% reframe(mean(q95_prd.stream_temp, na.rm = T))
  dd <- cbind(dd1, dd2[,2], dd3[,2]); colnames(dd) <- c("doy", "mean_prd.stream_temp", "q05_prd.stream_temp", "q95_prd.stream_temp")
  q05_9020 <- predict(loess(dd$q05_prd.stream_temp ~ dd$doy, span = 0.5))
  q95_9020 <- predict(loess(dd$q95_prd.stream_temp ~ dd$doy, span = 0.5))
  
  dd <- agg2020s[agg2020s$COMID %in% cids,]
  dd1 <- dd %>% group_by(doy) %>% reframe(mean(mean_STadj, na.rm = T))
  dd2 <- dd %>% group_by(doy) %>% reframe(mean(q05_STadj05, na.rm = T))
  dd3 <- dd %>% group_by(doy) %>% reframe(mean(q95_STadj95, na.rm = T))
  dd <- cbind(dd1, dd2[,2], dd3[,2]); colnames(dd) <- c("doy", "mean_STadj", "q05_STadj05", "q95_STadj95")
  q05_2020s <- predict(loess(dd$q05_STadj05 ~ dd$doy, span = 0.5))
  q95_2020s <- predict(loess(dd$q95_STadj95 ~ dd$doy, span = 0.5))
  
  dd <- agg2040s[agg2040s$COMID %in% cids,]
  dd1 <- dd %>% group_by(doy) %>% reframe(mean(mean_STadj, na.rm = T))
  dd2 <- dd %>% group_by(doy) %>% reframe(mean(q05_STadj05, na.rm = T))
  dd3 <- dd %>% group_by(doy) %>% reframe(mean(q95_STadj95, na.rm = T))
  dd <- cbind(dd1, dd2[,2], dd3[,2]); colnames(dd) <- c("doy", "mean_STadj", "q05_STadj05", "q95_STadj95")
  q05_2040s <- predict(loess(dd$q05_STadj05 ~ dd$doy, span = 0.5))
  q95_2040s <- predict(loess(dd$q95_STadj95 ~ dd$doy, span = 0.5))
  
  dd <- agg2060s[agg2060s$COMID %in% cids,]
  dd1 <- dd %>% group_by(doy) %>% reframe(mean(mean_STadj, na.rm = T))
  dd2 <- dd %>% group_by(doy) %>% reframe(mean(q05_STadj05, na.rm = T))
  dd3 <- dd %>% group_by(doy) %>% reframe(mean(q95_STadj95, na.rm = T))
  dd <- cbind(dd1, dd2[,2], dd3[,2]); colnames(dd) <- c("doy", "mean_STadj", "q05_STadj05", "q95_STadj95")
  q05_2060s <- predict(loess(dd$q05_STadj05 ~ dd$doy, span = 0.5))
  q95_2060s <- predict(loess(dd$q95_STadj95 ~ dd$doy, span = 0.5))
  
  dd <- agg2080s[agg2080s$COMID %in% cids,]
  dd1 <- dd %>% group_by(doy) %>% reframe(mean(mean_STadj, na.rm = T))
  dd2 <- dd %>% group_by(doy) %>% reframe(mean(q05_STadj05, na.rm = T))
  dd3 <- dd %>% group_by(doy) %>% reframe(mean(q95_STadj95, na.rm = T))
  dd <- cbind(dd1, dd2[,2], dd3[,2]); colnames(dd) <- c("doy", "mean_STadj", "q05_STadj05", "q95_STadj95")
  q05_2080s <- predict(loess(dd$q05_STadj05 ~ dd$doy, span = 0.5))
  q95_2080s <- predict(loess(dd$q95_STadj95 ~ dd$doy, span = 0.5))
  
  emp_agg1 <- as.data.frame(emp_agg[emp_agg$COMID %in% cids,])
  dd1 <- emp_agg1 %>% group_by(doy) %>% reframe(mean(mean_AvgDailyTemp, na.rm = T))
  dd2 <- emp_agg1 %>% group_by(doy) %>% reframe(mean(q05_AvgDailyTemp, na.rm = T))
  dd3 <- emp_agg1 %>% group_by(doy) %>% reframe(mean(q95_AvgDailyTemp, na.rm = T))
  emp_agg1 <- cbind(dd1, dd2[,2], dd3[,2]); colnames(emp_agg1) <- c("doy", "mean_AvgDailyTemp", "q05_AvgDailyTemp", "q95_AvgDailyTemp")
  q05_emp <- predict(loess(emp_agg1$q05_AvgDailyTemp ~ emp_agg1$doy, span = 0.5))
  q95_emp <- predict(loess(emp_agg1$q95_AvgDailyTemp ~ emp_agg1$doy, span = 0.5))

  colrs <- c("#FF990080", "#994F0080", "#0C7BDC80", "#0C9BAA80")
  ylms <- c(0,23)
  
  png(paste0("plots/annual_", nm, ".png"), width = 5.5, height = 4, units = "in", res = 300)
  plot(1:366, q95_emp, cex = .3, type = "n",
       main = nm, las = 1, ylab = "Stream temperature (C)", xlab = "Date", ylim = ylms)
  polygon(c(1:366, rev(1:366)), c(q05_2080s, rev(q95_2080s)), col = colrs[1], border = NA)
  polygon(c(1:366, rev(1:366)), c(q05_2060s, rev(q95_2060s)), col = colrs[2], border = NA)
  polygon(c(1:366, rev(1:366)), c(q05_2040s, rev(q95_2040s)), col = colrs[3], border = NA)
  polygon(c(1:366, rev(1:366)), c(q05_2020s, rev(q95_2020s)), col = colrs[4], border = NA)
  #polygon(c(1:366, rev(1:366)), c(q05_9020, rev(q95_9020)), col = colrs[4], border = NA)
  #empirical:
  lines(1:length(q05_emp), q05_emp)
  lines(1:length(q95_emp), q95_emp)

  abline(h = seq(0,20,5), lty = 3, col = "gray40")
  abline(v = pretty(dd$doy), lty = 3, col = "gray40")
  if(i == 1) {legend("topleft", legend = c("2080s", "2060s", "2040s", "2020s", "observed"), col = c(colrs, "black"), lwd = 2)}
  dev.off()  
}


# print quantiles of daily August temperatures
for(i in 1:5){
  if(i == 1) {cids <- spawn_reaches$COMID[spawn_reaches$spawn_reach %in% "white"]; nm <- "White River"} 
  if(i == 2) {cids <- spawn_reaches$COMID[spawn_reaches$spawn_reach %in% "chiwawa"]; nm <- "Chiwawa River"} 
  if(i == 3) {cids <- spawn_reaches$COMID[spawn_reaches$spawn_reach %in% "nason"]; nm <- "Nason Creek"}
  if(i == 4) {cids <- spawn_reaches$COMID[spawn_reaches$spawn_reach %in% "wenlower"]; nm <- "Wenatchee River"}
  if(i == 5) {cids <- spawn_reaches$COMID[spawn_reaches$spawn_reach %in% "wenmigr"]; nm <- "Migration Corridor"}
  
  cat(nm, quantile(fut_pds$STadj[lubridate::month(fut_pds$Date) %in% 8 & fut_pds$COMID %in% cids], probs = c(0,0.1, 0.5, 0.9, 1)), "\n")
}

# mean August metric by year
mnAugByYear <- fncAggBy(dat = fut_pds[, c("Date", "COMID", "STadj")], filter_month = 8, reach = COMID, timestep = year, stat = "mean")
mnAugByYear95 <- fncAggBy(dat = fut_pds[, c("Date", "COMID", "STadj95")], filter_month = 8, reach = COMID, timestep = year, stat = "q", p = 0.95)

data.table::fwrite(mnAugByYear, "data/mnAugByYear.csv")
data.table::fwrite(mnAugByYear95, "data/mnAugByYear95.csv")

# 
# colrs <- c("gray", "#0C9BAA80", "#0C7BDC80", "#FF990080", "#994F0080")
#   
#   dat2plot <- as.data.frame(mnAugByYear); var <- "mean_STadj"
#   #dat2plot <- as.data.frame(mnAugByYear95); var <- "q95_STadj95"
#   cids <- spawn_reaches$COMID[spawn_reaches$spawn_reach %in% "white"]
#   plot(dat2plot$year[dat2plot$COMID %in% cids], dat2plot[,var][dat2plot$COMID %in% cids], 
#        las = 1, ylab = "Stream temperature (C)", xlab = "Date", ylim = c(10,25), type = 'n')
#   cids <- spawn_reaches$COMID[spawn_reaches$spawn_reach %in% "chiwawa"]
#   points(dat2plot$year[dat2plot$COMID %in% cids], dat2plot[, var][dat2plot$COMID %in% cids], pch = 19, col = colrs[2])
#   cids <- spawn_reaches$COMID[spawn_reaches$spawn_reach %in% "nason"]
#   points(dat2plot$year[dat2plot$COMID %in% cids], dat2plot[, var][dat2plot$COMID %in% cids], pch = 19, col = colrs[3])
#   cids <- spawn_reaches$COMID[spawn_reaches$spawn_reach %in% "wenmigr"]
#   points(dat2plot$year[dat2plot$COMID %in% cids], dat2plot[, var][dat2plot$COMID %in% cids], pch = 19, col = colrs[4])
#   cids <- spawn_reaches$COMID[spawn_reaches$spawn_reach %in% "wenlower"]
#   points(dat2plot$year[dat2plot$COMID %in% cids], dat2plot[, var][dat2plot$COMID %in% cids], pch = 19, col = colrs[5])
#   cids <- spawn_reaches$COMID[spawn_reaches$spawn_reach %in% "white"]
#   points(dat2plot$year[dat2plot$COMID %in% cids], dat2plot[, var][dat2plot$COMID %in% cids], pch = 19, col = colrs[1])
#   abline(h = seq(10,26,2), lty = 3, col = "gray")
#   abline(v = seq(2020, 2100,10), lty = 3, col = "gray")
#   abline(h = c(17, 20))
#   
# # Get migration corridor
# library(nhdplusTools)
# streams <- sf::st_read("data/shp/Wenatchee_NHDv2_vat.shp")|>
#   sf::st_transform('+proj=longlat +datum=WGS84') |>
#   sf::st_zm()
# ds.comids <- get_DM(streams, 23080722)
# #us.comids <- get_UT(streams, start_comid)
# plot(sf::st_geometry(sf::st_zm(streams)), col = "gray")
# plot(sf::st_geometry(sf::st_zm(dplyr::filter(streams, COMID %in% ds.comids))), add = T,lwd = 2.5, col = "darkorange") #downstream reaches 

