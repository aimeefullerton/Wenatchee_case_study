# Map estimated Upper Columbia Chinook salmon prespawn mortality

library(sf)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(dplyr)

huc <- 17020011

xwalk <- read.csv("data/COMID_to_HUC12.csv")
xwalk$Huc08 <- substr(xwalk$Huc12, 1, 8)
c <- xwalk$COMID[xwalk$Huc08 == huc]

streams <- read_sf("data/shp/upper_columbia_streams_nhd2.shp")
spawn_reaches <- read.csv("data/Wenatchee_spawn_reaches.csv")
streams <- dplyr::left_join(streams, spawn_reaches, join_by(COMID))
cids <- spawn_reaches$COMID

wenbndry <- read_sf("data/shp/Wenatchee_boundary.shp")
wenstreams <- read_sf("data/shp/Wenatchee_NHDv2_SOgt2.shp")
streams <- dplyr::left_join(wenstreams, spawn_reaches, join_by(COMID))

metric <- "AugMn"
PSMdata <- data.table::fread(paste0("data/UC_PSM_", huc, "_", metric, ".csv"))[,-1]
PSMall <- data.table::fread(paste0("data/UC_PSM_", huc, "_", metric, "_all.csv"))[,-1]

# Aggregate across periods
dat <- PSMall[, c("COMID", "year", "ST.metric", "PSMPred", "psmlo", "psmhi")]
# Create a Period column based on the year
dat[, Period := cut(year,
                  breaks = c(1991, 2015, 2040, 2065, 2090),
                  labels = c(1, 2, 3, 4),
                  right = T,
                  include.lowest = T
)]
dat <- dat[!is.na(dat$Period),] # remove 1990 records
agg <- dat[, .(
  ST.metric = mean(ST.metric, na.rm = T),
  PSMPred = mean(PSMPred, na.rm = T),
  psmlo = mean(psmlo, na.rm = T),
  psmhi = mean(psmhi, na.rm = T)
), by = .(COMID, Period)]

# Calculate Deltas
hh <- subset(agg, Period == 1)
cc <- subset(agg, Period == 2)
nn <- subset(agg, Period == 3)
ff <- subset(agg, Period == 4)

new <- left_join(hh, cc, by = c("COMID"))
new <- left_join(new, nn, by = c("COMID"))
new <- left_join(new, ff, by = c("COMID"))
new <- new[, -c(2, 7, 12, 17)]
colnames(new) <- c("COMID", "ST1", "PSMmn1", "PSMlo1", "PSMhi1", "ST2", "PSMmn2", "PSMlo2", "PSMhi2", 
                   "ST3", "PSMmn3", "PSMlo3", "PSMhi3", "ST4", "PSMmn4", "PSMlo4", "PSMhi4")

# Delta mean: difference of period means
new$dc <- new$PSMhi2 - new$PSMhi1
new$dn <- new$PSMhi3 - new$PSMhi1
new$df <- new$PSMhi4 - new$PSMhi1

new.spwn <- new[new$COMID %in% cids,]

# join to streams
sf_data <- new %>%
  dplyr::left_join(streams, ., join_by(COMID)) 
spwn_rchs <- sf_data[sf_data$COMID %in% cids,]
wlwr <- sf_data[sf_data$COMID %in% wenlower,]


pal <- viridisLite::plasma(7)
fncPlotMap <- function(temp_column = prd.stream_temp, legpos = "none", legnm = "ΔPSM", lims = c(0,1)){
  plot_prd <- ggplot()+
    geom_sf(data = wenbndry, fill = "grey30", color = "grey30", size = 0.5) + 
    geom_sf(data = spwn_rchs, color = "white", linewidth = 1.5)+
    geom_sf(data = wlwr, color = "white", linewidth = 2.5)+
    geom_sf(data = sf_data, aes(color = {{temp_column}}, linewidth = StreamOrde))+
    scale_color_stepsn( 
      n.breaks = 7, colors = pal,
      limits = lims, name = legnm,
      na.value = "gray80")+
    scale_linewidth_continuous(range = c(0.2, 1.2)) +
    guides(linewidth = "none") +
    theme_void()+
    theme(legend.position = legpos) #+
    #theme(
    #  text = element_text(color = "white"),
    #  plot.background = element_rect(fill = "grey30", color =NA))
  return(plot_prd)
}

# deltas between periods
plot_1 <- fncPlotMap(dc, lims = c(0,0.28))
plot_2 <- fncPlotMap(dn, lims = c(0,0.28))
plot_3 <- fncPlotMap(df, legpos = "right", lims = c(0,0.28))

combo_plot <- cowplot::plot_grid( # combine plots
  plot_1, plot_2, plot_3,
  nrow = 1,
  rel_widths = c(0.8, 0.8, 1),
  #labels = c("Current", "Near future", "Far future"),
  label_colour = "black", label_size = 12)+
  theme(
    text = element_text(color = "black"),
    plot.background = element_rect(fill = "white", color =NA))

ggsave(filename = paste0("plots/", huc, "_", metric, "_PSMmaps_deltas.png"), combo_plot, height = 3)


# actual values for a specific year
yy <- 2000
PSM_yy <- PSMdata[PSMdata$year == yy,]
sf_data <- PSM_yy %>%
  dplyr::left_join(streams, ., join_by(COMID)) %>%
  filter(COMID %in% c)

plot_1 <- fncPlotMap(PSMPred)
plot_2 <- fncPlotMap(psmlo)
plot_3 <- fncPlotMap(psmhi, legpos = "right")

combo_plot <- cowplot::plot_grid( # combine plots
  plot_2, plot_1, plot_3,
  nrow = 1,
  rel_widths = c(0.8, 0.8, 1),
  labels = c("Low", "Mean", "High"),
  label_colour = "black", label_size = 12)+
  theme(
    text = element_text(color = "black"),
    plot.background = element_rect(fill = "white", color =NA))

ggsave(filename = paste0("plots/", huc, "_", metric, "_PSMmaps_", yy, ".png"), combo_plot, height = 3)



# Spawning tribs
plot_tribs <-
  ggplot()+
  geom_sf(data = wenbndry, fill = "grey30", color = "grey30", size = 0.5) + 
  geom_sf(data = sf_data, aes(color = spawn_reach, linewidth = StreamOrde))+
  scale_linewidth_continuous(range = c(0.6, 1.2)) +
  guides(linewidth = "none") +
  theme_void()+
  theme(
    text = element_text(color = "black"),
    plot.background = element_rect(fill = "white", color =NA))

ggsave(filename = paste0("plots/", huc, "_SpawnReaches.png"), plot_tribs, height = 3)

