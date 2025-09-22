# Map thermal metrics

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

wenbndry <- read_sf("data/shp/Wenatchee_boundary.shp")
wenstreams <- read_sf("data/shp/Wenatchee_NHDv2_SOgt2.shp")
streams <- dplyr::left_join(wenstreams, spawn_reaches, join_by(COMID))

DADM7 <- data.table::fread(paste0("data/7DADM_", huc, ".csv"))[,-1]
AugMn <- data.table::fread(paste0("data/AugMn_", huc, ".csv"))[,-1]

pal <- viridisLite::plasma(7)
fncPlotMap <- function(temp_column = prd.stream_temp, legpos = "none", legnm = "7DADM", lims = c(5,34)){
  plot_prd <- ggplot()+
    geom_sf(data = wenbndry, fill = "grey30", color = "grey30", size = 0.5) + 
    geom_sf(data = sf_data, aes(color = {{temp_column}}, linewidth = StreamOrde))+
    scale_color_stepsn(
      n.breaks = 7, colors = pal,
      limits = lims, name = legnm,
      na.value = "gray80")+
    scale_linewidth_continuous(range = c(0.2, 1.2)) +
    guides(linewidth = "none") +
    theme_void()+
    theme(legend.position = legpos)
  return(plot_prd)
}

# 7DADM ----
yy <- 2000
stmet1 <- DADM7[DADM7$year == yy,]
sf_data <- stmet1 %>%
  dplyr::left_join(streams, ., join_by(COMID)) %>%
  filter(COMID %in% c)


plot_prd.mx <- fncPlotMap(prd.stream_temp.mx, lims = c(0,28))
plot_c.mx <- fncPlotMap(st.c.mnmx, lims = c(0,28))
plot_nf.mx <- fncPlotMap(st.nf.mnmx, lims = c(0,28))
plot_ff.mx <- fncPlotMap(st.ff.mnmx, legpos = "right", lims = c(0,28))


combo_plot <- cowplot::plot_grid( # combine plots
 plot_prd.mx, plot_c.mx, plot_nf.mx, plot_ff.mx,
 labels = c("2000", "2025", "2050", "2075"),
 label_colour = "black", label_size = 12)+
  theme(
    text = element_text(color = "black"),
    plot.background = element_rect(fill = "white", color =NA))

ggsave(filename = paste0("plots/maps_7DADM_", huc, ".png"), combo_plot)


# Map just the deltas:
sf_data$delta.c <- sf_data$st.c.mnmx  - sf_data$prd.stream_temp.mx
sf_data$delta.nf <- sf_data$st.nf.mnmx  - sf_data$prd.stream_temp.mx
sf_data$delta.ff <- sf_data$st.ff.mnmx  - sf_data$prd.stream_temp.mx

plot_c.mx <- fncPlotMap(delta.c, legnm = "delta 7DADM", lims = c(0,3))
plot_nf.mx <- fncPlotMap(delta.nf, legnm = "delta 7DADM", lims = c(0,3))
plot_ff.mx <- fncPlotMap(delta.ff, legpos = "right", legnm = "delta 7DADM", lims = c(0,3))

combo_plot <- cowplot::plot_grid( # combine plots
  plot_c.mx, plot_nf.mx, plot_ff.mx,
  nrow = 1, rel_widths = c(0.68, 0.68, 1),
  labels = c("current", "near future", "far future"),
  label_colour = "black", label_size = 12)+
  theme(
    text = element_text(color = "black"),
    plot.background = element_rect(fill = "white", color =NA))

ggsave(filename = paste0("plots/maps_delta7DADM_", huc, ".png"), combo_plot)


# AugMn ----
yy <- 2000
stmet1 <- AugMn[AugMn$year == yy,]
sf_data <- stmet1 %>%
  dplyr::left_join(streams, ., join_by(COMID)) %>%
  filter(COMID %in% c)


plot_prd <- fncPlotMap(prd.stream_temp, lims = c(0,25))
plot_c <- fncPlotMap(cmn, lims = c(0,25))
plot_nf <- fncPlotMap(nmn, lims = c(0,25))
plot_ff <- fncPlotMap(fmn, legpos = "right", legnm = "AugMn", lims = c(0,25))

combo_plot <- cowplot::plot_grid( # combine plots
  plot_prd, plot_c, plot_nf, plot_ff,
  labels = c("2000", "2025", "2050", "2075"),
  label_colour = "black", label_size = 12)+
  theme(
    text = element_text(color = "black"),
    plot.background = element_rect(fill = "white", color =NA))

ggsave(filename = paste0("plots/maps_AugMn_", huc, ".png"), combo_plot)


# Map just the deltas:
sf_data$delta.c <- sf_data$cmn  - sf_data$prd.stream_temp
sf_data$delta.nf <- sf_data$nmn  - sf_data$prd.stream_temp
sf_data$delta.ff <- sf_data$fmn  - sf_data$prd.stream_temp

plot_dc <- fncPlotMap(delta.c, legnm = "delta MnAug", lims = c(0,3))
plot_dnf <- fncPlotMap(delta.nf, legnm = "delta MnAug", lims = c(0,3))
plot_dff <- fncPlotMap(delta.ff, legpos = "right", legnm = "delta MnAug", lims = c(0,3))

combo_plot <- cowplot::plot_grid( # combine plots
  plot_dc, plot_dnf, plot_dff,
  nrow = 1, rel_widths = c(0.68, 0.68, 1),
  labels = c("current", "near future", "far future"),
  label_colour = "black", label_size = 12)+
  theme(
    text = element_text(color = "black"),
    plot.background = element_rect(fill = "white", color =NA))

ggsave(filename = paste0("plots/maps_deltaAugMn_", huc, ".png"), combo_plot)

