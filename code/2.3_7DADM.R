# Summarize 7DADM for stream temperature data
library(tidyverse)
library(RColorBrewer)

huc <- 17020011

tdat <- data.table::fread(paste0("data/ST_max_", huc, ".csv"))

# Get 7DADM and the doy on which the week occurred (center day)
calc_7DADM <- function(dat, temp_column){
  dat |>
    mutate(year = lubridate::year(tim.date)) |>
    mutate(
      rollmeanC = zoo::rollmean({{temp_column}}, 7, fill = NA),
      .by = c(COMID, year)) |>
    group_by(COMID, year) |> # Group before summarizing to find max and then filter
    summarize(
      DADM7_pred = max(rollmeanC, na.rm = TRUE),
      doy_max_rollmean = tim.date[which.max(rollmeanC)], # Get the date corresponding to the max
      .groups = 'drop' # Drop grouping after summarizing
    ) |>
    mutate(doy_max_rollmean = lubridate::yday(doy_max_rollmean)) # Convert date to day of year
}

DADM7 <- cbind(calc_7DADM(dat = tdat, temp_column = prd.stream_temp), calc_7DADM(dat = tdat, temp_column = prd.stream_temp.mx),
               calc_7DADM(dat = tdat, temp_column = st.c.mnmx), calc_7DADM(dat = tdat, temp_column = st.nf.mnmx), calc_7DADM(dat = tdat, temp_column = st.ff.mnmx))
DADM7 <- DADM7[, c(1:3,7,11,15,19)]; colnames(DADM7) <- c("COMID", "year", "prd.stream_temp", "prd.stream_temp.mx", "st.c.mnmx", "st.nf.mnmx", "st.ff.mnmx")


# Examine and save ----
hist(DADM7$prd.stream_temp, breaks = 40)
hist(DADM7$prd.stream_temp.mx, breaks = 40)
hist(DADM7$st.ff.mnmx, breaks = 40)

write.csv(DADM7, paste0("data/7DADM_", huc, ".csv"))

# Collect into a long format ----
td <- DADM7 %>% pivot_longer(cols = prd.stream_temp.mx:st.ff.mnmx, names_to = "era", values_to = "prd.7DADM")
td$year2 <- td$year
td$year2[td$era %in% "st.c.mnmx"] <- td$year[td$era %in% "st.c.mnmx"] + 25
td$year2[td$era %in% "st.nf.mnmx"] <- td$year[td$era %in% "st.nf.mnmx"] + 50
td$year2[td$era %in% "st.ff.mnmx"] <- td$year[td$era %in% "st.ff.mnmx"] + 75
td <- td[, c(1,6,5)]
colnames(td) <- c("COMID", "year", "prd.7DADM")
result <- td

# Add low and high estimates ----
DADM7lo <- cbind(calc_7DADM(dat = tdat, temp_column = st.c.lomx), calc_7DADM(dat = tdat, temp_column = st.nf.lomx), calc_7DADM(dat = tdat, temp_column = st.ff.lomx))
DADM7lo <- DADM7lo[, c(1:3,7,11)]; colnames(DADM7lo) <- c("COMID", "year", "st.c.lomx", "st.nf.lomx", "st.ff.lomx")
DADM7hi <- cbind(calc_7DADM(dat = tdat, temp_column = st.c.himx), calc_7DADM(dat = tdat, temp_column = st.nf.himx), calc_7DADM(dat = tdat, temp_column = st.ff.himx))
DADM7hi <- DADM7hi[, c(1:3,7,11)]; colnames(DADM7hi) <- c("COMID", "year", "st.c.himx", "st.nf.himx", "st.ff.himx")

td <- DADM7lo %>% pivot_longer(cols = st.c.lomx:st.ff.lomx, names_to = "era", values_to = "prd.7DADM")
td$year2 <- td$year
td$year2[td$era %in% "st.c.lomx"] <- td$year[td$era %in% "st.c.lomx"] + 25
td$year2[td$era %in% "st.nf.lomx"] <- td$year[td$era %in% "st.nf.lomx"] + 50
td$year2[td$era %in% "st.ff.lomx"] <- td$year[td$era %in% "st.ff.lomx"] + 75
td <- td[, c(1,5,4)]
colnames(td) <- c("COMID", "year", "prd.7DADM")
result <- left_join(result, td, by = c("COMID", "year"))

td <- DADM7hi %>% pivot_longer(cols = st.c.himx:st.ff.himx, names_to = "era", values_to = "prd.7DADM")
td$year2 <- td$year
td$year2[td$era %in% "st.c.himx"] <- td$year[td$era %in% "st.c.himx"] + 25
td$year2[td$era %in% "st.nf.himx"] <- td$year[td$era %in% "st.nf.himx"] + 50
td$year2[td$era %in% "st.ff.himx"] <- td$year[td$era %in% "st.ff.himx"] + 75
td <- td[, c(1,5,4)]
colnames(td) <- c("COMID", "year", "prd.7DADM")
result <- left_join(result, td, by = c("COMID", "year"))
colnames(result)[3:5] <- c("prd.7DADM", "prd.7DADM.lo", "prd.7DADM.hi")


# Examine and save ----
write.csv(result, paste0("data/prd.7DADM_", huc, ".csv"))

foo <- result[result$COMID %in% unique(result$COMID)[1],]
foo <- foo[order(foo$year),]
plot(foo$year, foo$prd.7DADM, type = 'b')
points(foo$year, foo$prd.7DADM.hi, type = 'b', col = 2)
points(foo$year, foo$prd.7DADM.lo, type = 'b', col = 4)

# Compare to mean August ----
AugMn <- read.csv(paste0("data/AugMn_", huc, ".csv"))
foo <- merge(DADM7, AugMn, by = c("COMID", "year"), all.x = T)
plot(foo$prd.stream_temp.mx ~ foo$prd.stream_temp.y, ylab = "7DADM", xlab = "August mean", las = 1, cex = 0.4)
abline(a = 0, b = 1, col = 2, lwd = 2)
m <- lm(foo$prd.stream_temp.mx ~ foo$prd.stream_temp.y) #Adjusted R-squared:  0.95
a = m$coefficients[1]; b = m$coefficients[2]
legend("topleft", legend = paste0("R^2 = ", round(summary(m)$adj.r.squared,3)), bty = 'n')

plot(DADM7$prd.stream_temp.mx, DADM7$st.c.mnmx)
abline(a = 0, b = 1, col = 2)
points(DADM7$prd.stream_temp.mx, DADM7$st.ff.mnmx, col = 2)
points(DADM7$prd.stream_temp.mx, DADM7$st.nf.mnmx, col = 3)

colrs <- rev(brewer.pal(6,"Greens"))[2:5]
colrs <- scales::alpha(colrs, alpha = 0.5)

boxplot(DADM7$prd.stream_temp.mx ~ DADM7$year, outline = F, col = colrs[1], border = colrs[1], las = 1, ylab = "7DADM", xlab = "year")
boxplot(DADM7$st.c.mnmx ~ DADM7$year, outline = F, col = colrs[2], , border = colrs[2], add = T, axes = F)
boxplot(DADM7$st.nf.mnmx ~ DADM7$year, outline = F, col = colrs[3], border = colrs[3], add = T, axes = F)
boxplot(DADM7$st.ff.mnmx ~ DADM7$year, outline = F, col = colrs[4], border = colrs[4], add = T, axes = F)



