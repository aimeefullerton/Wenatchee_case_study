# Calculate a delta stream temperature for future periods

# Load predictions from the localized model
st <- data.table::fread("data/st_pred_UC.csv")
st$year <- lubridate::year(st$tim.date)
st$doy <- lubridate::yday(st$tim.date)
st <- st[!is.na(st$prd.stream_temp),]
st <- st[,c("COMID", "tim.date", "year", "doy", "prd.stream_temp")]

# Get future scenario data
setwd("cc_preds")
fls <- dir(getwd())
fl <- fls[grep(".csv", fls)]
basin <- 17020011
fl <- fl[grep(basin, fl)]
climate_scenarios <- c("CanESM2", "CCSM4", "CNRM-CM5", "CSIRO-Mk3-6-0", "GDFL-ESM2M", "HadGEM2-CC", "HadGEM2-ES", "inmcm4", "IPSL-CM5A-MR", "MIROC")
climate_scenarios <- gsub("-", "_", climate_scenarios)
years <- 1990:2099

# Load the data.table library
library(data.table)
set.seed(123) # For reproducibility

for(huc10 in 1:length(fl)){
  
# 1. Summarize/aggregate data across periods and GCMs ----
  
  #Prepare data table  ----
  huc10_data <- data.table::fread(fl[huc10])
  huc10_data$year <- lubridate::year(huc10_data$date)
  huc10_data$doy <- lubridate::yday(huc10_data$date)
  cids <- sort(unique(huc10_data$COMID))

  cols <- c("date", "year", "doy", "COMID", climate_scenarios)
  f <- huc10_data[,..cols]
  f <- subset(f, year >= 1991) # exclude older data we have no need for
  f <- subset(f, year <= 2090) # exclude last decade so periods are equally distributed in 25-year increments
  f <- subset(f, !is.na(MIROC)) # exclude rows with no predictions
  
  # Identify the columns that contain the values for calculation (columns 3 to 12)
  value_cols <- names(f)[5:14]
  
  # Create a Period column based on the year
  f[, Period := cut(year,
              breaks = c(1991, 2015, 2040, 2065, 2090),
              labels = c(1, 2, 3, 4),
              right = TRUE,
              include.lowest = TRUE
  )]
  #table(f$year, f$Period)
  # 1: 1991-2015, 2: 2016-2040, 3: 2041-2065, 4: 2066-2090

# Calculate the mean and standard deviation for each day, location, and period
  # Melt the data into a long format
  long_f <- melt(f,
         id.vars = c("COMID", "Period", "doy"),
         measure.vars = value_cols,
         value.name = "value"
  )
  
  # Aggregate the data to calculate both mean and sd
  agg <- long_f[, .(
    aggmean = mean(value, na.rm = TRUE),
    aggsd = sd(value, na.rm = TRUE)
  ), by = .(COMID, Period, doy)]
  
  #plot(agg$doy[agg$Period == 1], agg$aggmean[agg$Period == 1])
  #points(agg$doy[agg$Period == 4], agg$aggmean[agg$Period == 4], col = 2)
    
  # Calculate Deltas
    hh <- subset(agg, Period == 1)
    cc <- subset(agg, Period == 2)
    nn <- subset(agg, Period == 3)
    ff <- subset(agg, Period == 4)
    
    library(dplyr)
    new <- left_join(hh, cc, by = c("COMID", "doy"))
    new <- left_join(new, nn, by = c("COMID", "doy"))
    new <- left_join(new, ff, by = c("COMID", "doy"))
    new <- new[, c("COMID", "doy", "aggmean.x", "aggsd.x", "aggmean.y", "aggsd.y", "aggmean.x.x", "aggsd.x.x", "aggmean.y.y", "aggsd.y.y")]
    colnames(new) <- c("COMID", "doy", "aggmean1", "aggsd1", "aggmean2", "aggsd2", "aggmean3", "aggsd3", "aggmean4", "aggsd4")
    
    # Delta mean: difference of period means
    new$dc <- new$aggmean2 - new$aggmean1
    new$dn <- new$aggmean3 - new$aggmean1
    new$df <- new$aggmean4 - new$aggmean1
    # Delta sd: sum of period standard deviations
    new$sc <- new$aggsd2 + new$aggsd1
    new$sn <- new$aggsd3 + new$aggsd1
    new$sf <- new$aggsd4 + new$aggsd1

    colnames(new) <- c("COMID", "doy", "h.mn", "h.sd", "c.mn", "c.sd", "n.mn", "n.sd", "f.mn", "f.sd", 
                       "mean.delta.c", "mean.delta.n", "mean.delta.f",
                       "sd.delta.c", "sd.delta.n", "sd.delta.f")


# 2. Add deltas to current predicted temperatures from localized model ----

  # Perform a non-equi join and update by reference
  # join 'st' with 'result' based on matching 'COMID' and 'doy' and then add/modify the columns directly within 'st'.
  out <- st[new[, c(1,2,11:16)], on = .(COMID, doy), allow.cartesian = TRUE]
  #setdiff(unique(result$COMID), unique(st$COMID))
  out <- subset(out, !is.na(out$prd.stream_temp))
  
  # This adds mean to existing predictions (trends and wiggles will be same as that year in prediction)
  out$cmn <- out$prd.stream_temp + out$mean.delta.c
  out$clo <- out$cmn - out$sd.delta.c
  out$chi <- out$cmn + out$sd.delta.c
  out$cmn[out$cmn < 0] <- 0 # force non-negative temperature
  out$clo[out$clo < 0] <- 0
  
  out$nmn <- out$prd.stream_temp + out$mean.delta.n
  out$nlo <- out$nmn - out$sd.delta.n
  out$nhi <- out$nmn + out$sd.delta.n
  out$nmn[out$nmn < 0] <- 0 # force non-negative temperature
  out$nlo[out$nlo < 0] <- 0
  
  out$fmn <- out$prd.stream_temp + out$mean.delta.f
  out$flo <- out$fmn - out$sd.delta.f
  out$fhi <- out$fmn + out$sd.delta.f
  out$fmn[out$fmn < 0] <- 0 # force non-negative temperature
  out$flo[out$flo < 0] <- 0
  
  data.table::fwrite(out, paste0("../../UC_PSM/ST_delta_", fl[huc10]))
  
  # # Instead, calculate delta from a sample of possibilities:
  # Note: not convinced this improves on the estimate, and is rather slow
  #
  # fncSmpl_vectorized <- function(dat, pcol = "prd.stream_temp", mcol = "mean.delta.c", scol = "sd.delta.c", N = 100){
  #   # Extract the columns as vectors
  #   dat = as.data.frame(dat)
  #   p <- as.numeric(dat[,pcol])
  #   m <- as.numeric(dat[,mcol])
  #   s <- as.numeric(dat[,scol])
  #   
  #   # Generate random numbers for each row in a single call
  #   # We use outer() to create a matrix where each row corresponds to
  #   # the p[i] + rnorm(..., m[i], s[i]) calculation.
  #   results_matrix <- outer(p, rnorm(n = N, mean = m, sd = s), FUN = "+")
  #   
  #   # Calculate the mean across each row of the results_matrix
  #   # This gives you a single result for each row of the original data.
  #   return(list("means" = rowMeans(results_matrix), "mins" = apply(results_matrix, 1, quantile, probs = 0.1), "maxs" = apply(results_matrix, 1, quantile, probs = 0.)))
  # }
  # 
  # c <- fncSmpl_vectorized(out, mcol = "mean.delta.c", scol = "sd.delta.c", N = 1000)
  # out$cmn <- c$means
  # out$clo <- c$mins
  # out$chi <- c$maxs
  # out$cmn[out$cmn < 0] <- 0 # force non-negative temperature
  # out$clo[out$clo < 0] <- 0
  # 
  # n <- fncSmpl_vectorized(out, mcol = "mean.delta.n", scol = "sd.delta.n", N = 1000)
  # out$nmn <- n$means
  # out$nlo <- n$mins
  # out$nhi <- n$maxs
  # out$nmn[out$nmn < 0] <- 0 # force non-negative temperature
  # out$nlo[out$nlo < 0] <- 0
  # 
  # f <- fncSmpl_vectorized(out, mcol = "mean.delta.f", scol = "sd.delta.f", N = 1000)
  # out$fmn <- f$means
  # out$flo <- f$mins
  # out$fhi <- f$maxs
  # out$fmn[out$fmn < 0] <- 0 # force non-negative temperature
  # out$flo[out$flo < 0] <- 0
  
  
  # # quick peek
  # foo <- out[out$doy %in% 200,]
  # hist(foo$prd.stream_temp, breaks = 50)
  # hist(foo$cmn, breaks = 50, col = rgb(0,0,1,0.5), add = T)
  # hist(foo$nmn, breaks = 50, col = rgb(0,1,0,0.5), add = T)
  # hist(foo$fmn, breaks = 50, col = rgb(1,0,0,0.5), add = T)
  # 
  # plot(foo$prd.stream_temp, foo$cmn)
  # points(foo$prd.stream_temp, foo$nmn, col = rgb(0,1,0,0.5))
  # points(foo$prd.stream_temp, foo$fmn, col = rgb(1,0,0,0.5))
  # abline(a = 0, b = 1, lwd = 4, col = "blue")
  # 
  # foo <- foo[foo$COMID %in% cids[1],]
  # plot(foo$year, foo$prd.stream_temp, type = 'b', ylim = c(10,24))
  # points(foo$year, foo$cmn, type = 'b', col = 4)
  # points(foo$year, foo$nmn, type = 'b', col = 3)
  # points(foo$year, foo$fmn, type = 'b', col = 2)
}

st <- NULL
for(i in 1:length(fl)){
  td <- data.table::fread(paste0("ST_delta_", fl[i]))
  st <- rbind(st, td)
}
data.table::fwrite(st, paste0("ST_delta_", basin, ".csv"))

# # Examine QAQC
#foo <- st[is.na(st$fmn),]
#table(foo$COMID, foo$doy) # a few days for 2 COMIDs NA in future period 4
# foo <- st[st$prd.stream_temp >25,]
# summary(foo$prd.stream_temp)
# unique(foo$COMID)
# # evaluated in ArcGIS as a few tiny connector pieces, a headwater of Chiwaukum Cr, and some lower Wenatchee braids

y <- 2010
# pick one:
cid <- 23080840; nm <- "Wenatchee River" #wenlower
cid <- 23080724; nm <- "Chiwawa River" #chiwawa
cid <- 23080878; nm <- "White River" #white
cid <- 23081086; nm <- "Nason Creek" #nason

st_iy <- st[st$year == y & st$COMID %in% cid,]
st_iy <- st_iy[order(st_iy$tim.date),]
ylms <- c(0, 23)
colrs <- c("#C67B9F80", "#B4BA1280", "#488AC780")

png(paste0("plots/COMID_", cid, "_", y, ".png"), width = 6, height = 5, units = "in", res = 300)
plot(st_iy$tim.date, st_iy$fmn, cex = .3, type = "n",
     main = paste0(nm, " (COMID ", cid, ")"), las = 1, ylab = "Stream temperature (C)", xlab = "Date", ylim = ylms)
polygon(c(st_iy$tim.date, rev(st_iy$tim.date)), c(st_iy$flo, rev(st_iy$fhi)), col = colrs[1], border = NA)
polygon(c(st_iy$tim.date, rev(st_iy$tim.date)), c(st_iy$nlo, rev(st_iy$nhi)), col = colrs[2], border = NA)
polygon(c(st_iy$tim.date, rev(st_iy$tim.date)), c(st_iy$clo, rev(st_iy$chi)), col = colrs[3], border = NA)
lines(st_iy$tim.date, st_iy$prd.stream_temp, cex = .3, col = "black")
abline(h = seq(0,20,5), lty = 3, col = "gray40")
abline(v = pretty(st_iy$tim.date), lty = 3, col = "gray40")
legend("topleft", legend = c("far future", "near future", "current", "predicted"), col = c(colrs, "black"), lwd = 2)
dev.off()
