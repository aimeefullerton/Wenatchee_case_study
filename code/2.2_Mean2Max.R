# Predict MAX daily based on empirical relationship with daily mean

huc <- 17020011

# 1. Get empirical data for constructing relationship ----

data_means_path <- "data/freeflow_data.fst"
data_max_path <- "data/freeflow_data_max.fst"
freeflow_means <- fst::read.fst(data_means_path)
freeflow_max <- fst::read.fst(data_max_path)
td1 <- freeflow_means[,c("tim.date", "COMID", "obs.stream_temp_daily_mean")]
td2 <- freeflow_max[,c("tim.date", "COMID", "obs.stream_temp_daily_max")]
td <- merge(td1, td2, by = c("tim.date", "COMID"))

plot(td$obs.stream_temp_daily_mean, td$obs.stream_temp_daily_max, xlab = "Observed mean daily temperature (C)", ylab = "Observed max daily temperature (C)")
m1 <- lm(td$obs.stream_temp_daily_max ~ td$obs.stream_temp_daily_mean)
summary(m1)
a = m1$coefficients[1]; b = m1$coefficients[2]
abline(a, b, lwd = 2, col = 2)
legend("topleft", legend = paste0("R^2 = ", round(summary(m1)$adj.r.squared,3)), bty = 'n')

# histograms - big jump at zero values for both
h <- hist(td$obs.stream_temp_daily_mean, breaks = 50)
h2 <- hist(td$obs.stream_temp_daily_max, breaks = 50)

# For just the COMIDs in question
spawn_reaches <- read.csv("data/Wenatchee_spawn_reaches.csv")
cids <- spawn_reaches$COMID
td2 <- td[td$COMID %in% cids,]
plot(td2$obs.stream_temp_daily_mean, td2$obs.stream_temp_daily_max, xlab = "Observed mean daily temperature (C)", ylab = "Observed max daily temperature (C)")
m2 <- lm(td2$obs.stream_temp_daily_max ~ td2$obs.stream_temp_daily_mean)
summary(m2)
a = m2$coefficients[1]; b = m2$coefficients[2]
abline(a, b, lwd = 2, col = 2)
legend("topleft", legend = paste0("R^2 = ", round(summary(m2)$adj.r.squared,3)), bty = 'n')

# 2. Convert to max ----
st <- data.table::fread(paste0("ST_delta_", basin, ".csv"))

a <- 0.46; b <- 1.1 # applying this to Entiat and Methow too as fit to all data not too different
st$prd.stream_temp.mx <- st$prd.stream_temp * b + a
st$st.c.mnmx <- st$cmn * b + a
st$st.c.lomx <- st$clo * b + a
st$st.c.himx <- st$chi * b + a
st$st.nf.mnmx <- st$nmn * b + a
st$st.nf.lomx <- st$nlo * b + a
st$st.nf.himx <- st$nhi * b + a
st$st.ff.mnmx <- st$fmn * b + a
st$st.ff.lomx <- st$flo * b + a
st$st.ff.himx <- st$fhi * b + a


# 3. Examine and save ----
summary(st)
hist(st$prd.stream_temp.mx, breaks = 50)
st <- subset(st, prd.stream_temp.mx < 29) #remove high outliers (subjective)

data.table::fwrite(st, paste0("data/ST_max_", huc, ".csv"))

#examine:
boxplot(list(st$prd.stream_temp.mx, st$st.c.mnmx, st$st.nf.mnmx, st$st.ff.mnmx))
boxplot(prd.stream_temp.mx ~ COMID, data = st)
boxplot(prd.stream_temp.mx ~ year, data = st)

boxplot(c(st$st.ff.lomx, st$st.ff.himx) ~ c(st$doy,st$doy), outline = F, col = 2, border = 2)
boxplot(c(st$st.nf.lomx, st$st.nf.himx) ~ c(st$doy,st$doy), outline = F, add = T, col = 3, border = 3)
boxplot(c(st$st.c.lomx, st$st.c.himx) ~ c(st$doy,st$doy), outline = F, add = T, col = 5, border = 5)
boxplot(prd.stream_temp.mx ~ doy, data = st, outline = F, add = T, col = 4, border = 4)


