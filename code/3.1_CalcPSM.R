# Prespawn mortality calculation from:

################################################################################
##  Bowerman et al. 2021 Final top ranked models (by AIC) for Upper Columbia 
##  spring Chinook salmon PSM mortality 
#################################################################################

#TOPUpCSP <- glmer(PSM ~  cTemp + Origin +  cJ.Day +
#                    (1|Stream.section) + (1|Year),  
#                  data=UpCSP4, family=binomial)


library(ggplot2)
library(dplyr)

spawn_reaches <- read.csv("data/Wenatchee_spawn_reaches.csv")
cids <- spawn_reaches$COMID

fut_pds <- data.table::fread("data/future_adjusted.csv")
st <- fut_pds[fut_pds$COMID %in% cids & lubridate::month(fut_pds$Date) %in% 8,]


# Add origin, where hatchery = 1, natural = 0
set.seed(123)
st1 <- st
st1$Origin <- 0
st2 <- st
st2$Origin <- 1
st <- rbind(st1, st2); rm(st1, st2)

# Add length
cJ.Length <- round(seq(600, 1000, length = 20), 0)
st$Length <- rnorm(n = nrow(st), mean = mean(cJ.Length), sd = sd(cJ.Length))
# paper: length mean cm 81.36, sd 9.69
hist(st$Length); range(st$Length)
par(mfrow = c(2,1)); hist(st$Length[st$Origin == 0]); hist(st$Length[st$Origin == 1]); par(mfrow = c(1,1))

# Center the data
st$Temp <- (st[,"STadj"] - 16.47984)/2.556276
st$Lng <- (st$Length - 813.6361)/96.89191

# Add day (already centered)
cJ.Day <- round(seq(-12,12, length = 20),0)
st$Day <- rnorm(n = nrow(st), mean = mean(cJ.Day), sd = sd(cJ.Day))
#st$Day = sample(cJ.Day, nrow(st), T)

idx <- which(is.na(st[,"STadj"]))
if(length(idx) > 0) {st <- st[-idx,]}

# Model formula for the Upper Columbia model with the lowest AIC
Mod.Mat <- model.matrix(~Temp + Origin + Day, data = st)

#read in fixed effects
fixef.all <-  read.csv(file="data/UCpublishedCOEF.csv", row.names=1)
fixef <- fixef.all[,1]
# Spring Chinook parameter estimates from model-averaged estimates in Fig 4:
# Day -1, Length 0.5, Origin 1, Temp 1, Work 2; only Day, Origin and Temp significant

NewData <- data.frame(st, eta = Mod.Mat %*% fixef)
NewData <- with(NewData, data.frame(NewData, PSMPred = exp(eta) / (1+exp(eta)) ))   ## Predict PSM

## read in covariance matrix
vcov.mat <- as.matrix(read.csv(file="data/UCPublished_VCOV.csv", row.names=1))

# eta = X*beta + Z*b + eps

# #Get the standard errors (first need to subset data to avoid memory limit issues)
# Not used in our application
#
# VAR <- diag(Mod.Mat %*% vcov.mat %*% t(Mod.Mat))   ##these are variances (sqrt of diag elements)
# NewData <- data.frame(NewData, pse = sqrt(VAR))   ## pse is p std error
# 
# ## estimate across range of data high/low CIs
# NewData <- with(NewData,
#                 data.frame(NewData,
#                            psmlo = exp(eta - 1.96 * pse) / (1 + exp(eta - 1.96 * pse)),
#                            psmhi = exp(eta + 1.96 * pse) / (1 + exp(eta + 1.96 * pse))))


# Save data
write.csv(NewData, paste0("data/Wenatchee_PSM.csv"))



# Figure 7 ---- 
PSM <- data.table::fread(paste0("data/Wenatchee_PSM.csv"))
PSM$year <- lubridate::year(PSM$Date)
d2p <- PSM[PSM$Origin == 0, c("doy", "year", "COMID", "STadj", "PSMPred")]
d2p <- d2p[order(d2p$COMID, d2p$year, d2p$doy),]

# create 5-year bins and convert to factor so ggplot treats them as distinct groups
d2p$bin <- floor(d2p$year / 10) * 10
d2p$bin_label = factor(paste0(d2p$bin, "s"))
d2p <- d2p[order(d2p$STadj),]
d2p$bin2 <- floor(d2p$STadj / 5) * 5
d2p$bin_label2 = factor(d2p$bin2)

# Create violin plot
fncPlotPSM <- function(dat, i, var = bin_label, filenm = "by_year"){
  
if(i == 1) {cids <- spawn_reaches$COMID[spawn_reaches$spawn_reach %in% "white"]; nm <- "White River"} 
if(i == 2) {cids <- spawn_reaches$COMID[spawn_reaches$spawn_reach %in% "chiwawa"]; nm <- "Chiwawa River"} 
if(i == 3) {cids <- spawn_reaches$COMID[spawn_reaches$spawn_reach %in% "nason"]; nm <- "Nason Creek"}
if(i == 4) {cids <- spawn_reaches$COMID[spawn_reaches$spawn_reach %in% "wenlower"]; nm <- "Wenatchee River"}
if(i == 5) {cids <- spawn_reaches$COMID[spawn_reaches$spawn_reach %in% "wenmigr"]; nm <- "Migration Corridor"}
dat <- dat[dat$COMID %in% cids,]
  
p <- ggplot(dat, 
  aes(x = {{var}}, y = PSMPred)) +
  geom_violin(
    fill = "purple4", color = "purple4",
    trim = T, 
    alpha = 0.4,
    linewidth = 0.2) +
  # Add a boxplot inside the violin to show median and quartiles
  stat_boxplot(
    geom = "errorbar", 
    width = 0.1,            # Controls the width of the horizontal hatches
    color = "gray10", 
    linewidth = 0.5
  ) +
  geom_boxplot(width = 0.1, linewidth = 0.5, outlier.shape = NA,
    fill = "purple4", 
    color = "gray10",
    alpha = 0.7) +
  scale_y_continuous(limits = c(0, 1))+
  labs(
    title = nm,
    x = "10-Year Period",
    y = "Predicted prespawn mortality",
  ) +
  theme_minimal() +
  theme(axis.text = element_text(size = 14))+
  theme(axis.title = element_text(size = 14))+
  theme(axis.title.y = element_text(vjust = 3.5))+
  theme(axis.title.x = element_text(vjust = -1))+
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
  )+
  theme(legend.position = "none") # Hide legend as x-axis labels are sufficient

filename <- paste0("plots/Wenatchee_vPSM_", filenm, "_", nm, ".png")
ggsave(filename = filename, p, height = 3.5, width = 6)
return(p)
}
fncPlotPSMbyST <- function(dat, i){
  if(i == 1) {cids <- spawn_reaches$COMID[spawn_reaches$spawn_reach %in% "white"]; nm <- "White River"} 
  if(i == 2) {cids <- spawn_reaches$COMID[spawn_reaches$spawn_reach %in% "chiwawa"]; nm <- "Chiwawa River"} 
  if(i == 3) {cids <- spawn_reaches$COMID[spawn_reaches$spawn_reach %in% "nason"]; nm <- "Nason Creek"}
  if(i == 4) {cids <- spawn_reaches$COMID[spawn_reaches$spawn_reach %in% "wenlower"]; nm <- "Wenatchee River"}
  if(i == 5) {cids <- spawn_reaches$COMID[spawn_reaches$spawn_reach %in% "wenmigr"]; nm <- "Migration Corridor"}
  dat <- dat[dat$COMID %in% cids,]
  
  p <- dat |> 
    ggplot(aes(x = STadj, y = PSMPred)) +
    # 1. Scatterplot Layer (Continuous)
    geom_point(aes(color = year), size = 1, alpha = 0.05) +
    
    # 2. Violin Plot Layer (Discretized via grouping)
    # Use 'group = five_year_bin' to force discrete violins on a continuous scale
    geom_violin(
      aes(x = STadj, group = bin_label2), 
      fill = NA, 
      color = "black",
      trim = TRUE, 
      linewidth = 0.5,
      position = "identity" # Keeps them overlaid on the points
    ) +
    
    # Styling and Scales
    scale_color_gradient(low = "#4D9221", high = "#C51B7D") +
    scale_x_continuous(limits = c(7, 28)) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
      title = nm,
      x = "Daily August stream temperature (°C)",
      y = "Predicted pre-spawn mortality for wild Chinook salmon"
    ) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 14),
      axis.title.y = element_text(vjust = 3.5),
      axis.title.x = element_text(vjust = -1),
      # Reducing grid lines as requested previously
      panel.grid.minor = element_blank()
    )
  filename <- paste0("plots/Wenatchee_PSM_by_ST_", nm, ".png")
  ggsave(filename = filename, p, height = 3.5, width = 6)
  return(p)
}

for(i in 1:5){
  p_yr <- fncPlotPSM(d2p, i)
  assign(paste0("p_yr", i), p_yr)
  p_st <- fncPlotPSMbyST(d2p, i)
  assign(paste0("p_st", i), p_st)
}


# # by Origin
# ggplot(PSM, aes(x = STadj, y = PSMPred, color = rev(as.factor(Origin)), border = rev(as.factor(Origin)), alpha = 0.1), size = 0.1)+
#   geom_point()+
#   theme(legend.position = "none")
