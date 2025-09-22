# Summarize August mean stream temperature data
library(tidyverse)
library(RColorBrewer)

huc <- 17020011

tdat <- data.table::fread(paste0("data/ST_max_", huc, ".csv"))

# Get August mean temp
calc_AugMean <- function(dat, temp_column){
  dat |>
    mutate(
      month = lubridate::month(tim.date),
      year = lubridate::year(tim.date)) |>
    filter(month == 8) |>
    summarize(
      st_C_pred = mean({{temp_column}}), # generic name allows for mean or max
      .by = c(COMID, year))}

AugMn <- cbind(calc_AugMean(dat = tdat, temp_column = prd.stream_temp), calc_AugMean(dat = tdat, temp_column = cmn), 
               calc_AugMean(dat = tdat, temp_column = nmn), calc_AugMean(dat = tdat, temp_column = fmn))
AugMn <- AugMn[, c(1:3,6,9,12)]; colnames(AugMn) <- c("COMID", "year", "prd.stream_temp", "cmn", "nmn", "fmn")

# Examine and save ----
hist(AugMn$prd.stream_temp, breaks = 40)
hist(AugMn$cmn, breaks = 40, col = 4, add = T)
hist(AugMn$nmn, breaks = 40, col = 2, add = T)
hist(AugMn$fmn, breaks = 40, col = 3, add = T)

write.csv(AugMn, paste0("data/AugMn_", huc, ".csv"))

colrs <- rev(brewer.pal(6,"Greens"))[2:5]
colrs <- scales::alpha(colrs, alpha = 0.5)

boxplot(AugMn$prd.stream_temp ~ AugMn$year, outline = F, col = colrs[1], border = colrs[1], las = 1, ylab = "AugMn", xlab = "year")
boxplot(AugMn$cmn ~ AugMn$year, outline = F, col = colrs[2], , border = colrs[2], add = T, axes = F)
boxplot(AugMn$nmn ~ AugMn$year, outline = F, col = colrs[3], border = colrs[3], add = T, axes = F)
boxplot(AugMn$fmn ~ AugMn$year, outline = F, col = colrs[4], border = colrs[4], add = T, axes = F)


# Collect into a long format ----
td <- AugMn %>% pivot_longer(cols = prd.stream_temp:fmn, names_to = "era", values_to = "prd.AugMn")
td$year2 <- td$year
td$year2[td$era %in% "cmn"] <- td$year[td$era %in% "cmn"] + 25
td$year2[td$era %in% "nmn"] <- td$year[td$era %in% "nmn"] + 50
td$year2[td$era %in% "fmn"] <- td$year[td$era %in% "fmn"] + 75
td <- td[, c(1,5,4)]
colnames(td) <- c("COMID", "year", "prd.AugMn")
result <- td

# Add low and high estimates ----
AugMnlo <- cbind(calc_AugMean(dat = tdat, temp_column = clo), calc_AugMean(dat = tdat, temp_column = nlo), calc_AugMean(dat = tdat, temp_column = flo))
AugMnlo <- AugMnlo[, c(1:3,6,9)]; colnames(AugMnlo) <- c("COMID", "year", "clo", "nlo", "flo")
AugMnhi <- cbind(calc_AugMean(dat = tdat, temp_column = chi), calc_AugMean(dat = tdat, temp_column = nhi), calc_AugMean(dat = tdat, temp_column = fhi))
AugMnhi <- AugMnhi[, c(1:3,6,9)]; colnames(AugMnhi) <- c("COMID", "year", "chi", "nhi", "fhi")

td <- AugMnlo %>% pivot_longer(cols = clo:flo, names_to = "era", values_to = "prd.AugMn")
td$year2 <- td$year
td$year2[td$era %in% "clo"] <- td$year[td$era %in% "clo"] + 25
td$year2[td$era %in% "nlo"] <- td$year[td$era %in% "nlo"] + 50
td$year2[td$era %in% "flo"] <- td$year[td$era %in% "flo"] + 75
td <- td[, c(1,5,4)]
colnames(td) <- c("COMID", "year", "prd.AugMn")
result <- left_join(result, td, by = c("COMID", "year"))

td <- AugMnhi %>% pivot_longer(cols = chi:fhi, names_to = "era", values_to = "prd.AugMn")
td$year2 <- td$year
td$year2[td$era %in% "chi"] <- td$year[td$era %in% "chi"] + 25
td$year2[td$era %in% "nhi"] <- td$year[td$era %in% "nhi"] + 50
td$year2[td$era %in% "fhi"] <- td$year[td$era %in% "fhi"] + 75
td <- td[, c(1,5,4)]
colnames(td) <- c("COMID", "year", "prd.AugMn")
result <- left_join(result, td, by = c("COMID", "year"))
colnames(result)[3:5] <- c("prd.AugMn", "prd.AugMn.lo", "prd.AugMn.hi")


# Examine and save ----
write.csv(result, paste0("data/prd.AugMn_", huc, ".csv"))

foo <- result[result$COMID %in% unique(result$COMID)[10],]
foo <- foo[order(foo$year),]
plot(foo$year, foo$prd.AugMn, type = 'b')
points(foo$year, foo$prd.AugMn.hi, type = 'b', col = 2)
points(foo$year, foo$prd.AugMn.lo, type = 'b', col = 4)
