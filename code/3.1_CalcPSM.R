# Prespawn mortality calculation from:

################################################################################
##  Bowerman et al. 2021 Final top ranked models (by AIC) for Upper Columbia 
##  spring Chinook salmon PSM mortality 
#################################################################################

#TOPUpCSP <- glmer(PSM ~  cTemp + Origin +  cJ.Day +
#                    (1|Stream.section) + (1|Year),  
#                  data=UpCSP4, family=binomial)


library(ggplot2)

huc <- 17020011
spawn_reaches <- read.csv("data/Wenatchee_spawn_reaches.csv")
cids <- spawn_reaches$COMID
# NEED TO GET SPAWNING REACHES FOR ENTIAT AND METHOW TOO

metric <- "AugMn" #or AugMn.lo, AugMn.hi

stdat <- read.csv(paste0("prd.", metric, "_", huc, ".csv"))[,-1]
st <- stdat[stdat$COMID %in% cids,]
colnames(st)[3] <- "ST.metric"


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
st$Temp <- (st[,"ST.metric"] - 16.47984)/2.556276
st$Lng <- (st$Length - 813.6361)/96.89191

# Add day (already centered)
cJ.Day <- round(seq(-12,12, length = 20),0)
st$Day <- rnorm(n = nrow(st), mean = mean(cJ.Day), sd = sd(cJ.Day))
#st$Day = sample(cJ.Day, nrow(st), T)

idx <- which(is.na(st[,"ST.metric"]))
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

#Get the standard errors
VAR <- diag(Mod.Mat %*% vcov.mat %*% t(Mod.Mat))   ##these are variances (sqrt of diag elements)
NewData <- data.frame(NewData, pse = sqrt(VAR))   ## pse is p std error

## estimate across range of data high/low CIs
NewData <- with(NewData,
                data.frame(NewData,
                           psmlo = exp(eta - 1.96 * pse) / (1 + exp(eta - 1.96 * pse)),
                           psmhi = exp(eta + 1.96 * pse) / (1 + exp(eta + 1.96 * pse))))


# Save data
write.csv(NewData, paste0("data/UC_PSM_", huc, "_", metric, ".csv"))


# Visualize results with CIs of PSM

plot1 <- NewData |> subset(Origin == 0) |>
  ggplot(aes(x = ST.metric, y = psmhi, color = year))+
  scale_color_gradient(low = "blue", high = "red")+
  geom_point(size = 1, alpha = 0.3)+
  theme(axis.text = element_text(size=12))+
  theme(axis.title = element_text(size=12))+
  theme(axis.title.y = element_text(vjust = 3.5))+
  theme(axis.title.x = element_text(vjust = -1))+
  labs(x = "Mean August stream temperature (°C)")+
  labs(y = "Predicted pre-spawn mortality for wild Chinook salmon")+
  scale_x_continuous(limits = c(0, 25))+
  scale_y_continuous(limits = c(0, 1))

ggsave(filename = paste0("plots/", huc, "_", metric, "_PSM_all.png"), plot1, height = 4.25, width = 6)


# by Origin
ggplot(NewData, aes(x = ST.metric, y = PSMPred, color = rev(as.factor(Origin)), border = rev(as.factor(Origin)), alpha = 0.1), size = 0.1)+
  geom_point()+
  theme(legend.position = "none")


# Plot over time ----
colrs <- c("#C67B9F80", "#B4BA1280", "#488AC780")

png("plots/PSM_over_time_spwn.png", width = 6, height = 6, units = "in", res = 300)
  d2p <- NewData[order(NewData$year),]
  d2p <- d2p[d2p$Origin == 0,]
  d2p <- d2p[d2p$COMID %in% cids,]
  plot(d2p$year, d2p$PSMPred, type = 'n', las = 1, ylab = "Predicted pre-spawn mortality for wild Chinook salmon", xlab = "Year", ylim = c(0,1))
  points(d2p$year, d2p$psmhi, pch = 19, cex = 0.7, col = colrs[1])
  points(d2p$year, d2p$PSMPred, pch = 19, cex = 0.7, col = colrs[2])
  points(d2p$year, d2p$psmlo, pch = 19, cex = 0.7, col = colrs[3])
  legend("topleft", legend = c("95% CI", "prediction", "5% CI"), col = colrs, pch = 19)
dev.off()
















# OTHER ----
# HARP equation
 h <- -(1/6) * NewData$Temp + 4 # for temps between 18 and 24


# HARP using Tracy's Willamette equation (I think)
willamette_ps <- function(temp, # taken from HARP
                            phos = 0,
                            b0 = -9.053,
                            b1 = .387,
                            b2 = .029) {
  log_p <- b0 + b1 * temp + b2 * phos
  p <- plogis(log_p)
  return(p)
}

NewData$wpsm <- willamette_ps(temp = NewData$prd.7DADM, phos = NewData$Origin)
ggplot(NewData, aes(x = prd.mean, y=wpsm))+
  geom_point(aes(x = prd.mean, y=wpsm))


