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


mnAugByYear <- as.data.frame(data.table::fread("data/mnAugByYear.csv"))
mnAugByYear$bin <- floor(mnAugByYear$year / 10) * 10
mnAugByYear$bin_label = factor(mnAugByYear$bin)

pal <- viridisLite::plasma(7)
fncPlotMap <- function(temp_column = mean_STadj, yy, legpos = "none", legnm = "", lims = c(4,25)){
  
  sf_data <- mnAugByYear[mnAugByYear$bin %in% yy,] %>%
    dplyr::left_join(streams, ., join_by(COMID)) 
  
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
    #labs(title = yy)
  return(plot_prd)
}

p_2020 <- fncPlotMap(yy = 2020, legpos = "right")
for(y in seq(2030, 2100, 10)){
  p_yr <- fncPlotMap(yy = y)
  assign(paste0("p_", y), p_yr)
}


combo_plot <- cowplot::plot_grid( # combine plots
 #p_2020, p_2010, p_2020, p_2030, p_2040, p_2050, p_2060, p_2070, p_2080, p_2090,
 p_2020, p_2040, p_2060, p_2080,
 labels = c("2020s", "2040s", "2060s", "2080s"),
 label_colour = "black", label_size = 12)+
  theme(
    text = element_text(color = "black"),
    plot.background = element_rect(fill = "white", color =NA))

ggsave(filename = paste0("plots/Wenatchee_mnAug_maps.png"), combo_plot)

