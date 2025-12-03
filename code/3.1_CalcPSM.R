# Prespawn mortality calculation from:

################################################################################
##  Bowerman et al. 2021 Final top ranked models (by AIC) for Upper Columbia 
##  spring Chinook salmon PSM mortality 
#################################################################################

#TOPUpCSP <- glmer(PSM ~  cTemp + Origin +  cJ.Day +
#                    (1|Stream.section) + (1|Year),  
#                  data=UpCSP4, family=binomial)


library(ggplot2)
library(rlang)
library(dplyr)

huc <- 17020011
spawn_reaches <- read.csv("data/Wenatchee_spawn_reaches.csv")
cids <- spawn_reaches$COMID
# NEED TO GET SPAWNING REACHES FOR ENTIAT AND METHOW TOO

metric <- "AugMn" #or AugMn.lo, AugMn.hi

stdat <- read.csv(paste0("data/prd.", metric, "_", huc, ".csv"))[,-1]
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



# Figure 7 ---- 
NewData <- data.table::fread(paste0("data/UC_PSM_", huc, "_", metric, ".csv"))
d2p <- NewData[NewData$Origin == 0, c("year", "COMID", "ST.metric", "PSMPred", "psmlo", "psmhi")]
d2p <- d2p[order(d2p$year, d2p$COMID),]
#d2p <- d2p[d2p$COMID %in% cids,]

yrs <- sort(unique(d2p$year))
result <- d2p %>% 
  group_by(year) %>%
  reframe(quantile(psmhi, c(0.05, 0.5, 0.95), na.rm = T), prob = c(0.05, 0.5, 0.95))
colnames(result) <- c("year", "psmhi", "quantile")

psm.min <- result[result$quantile == 0.05,-ncol(result)]
psm.max <- result[result$quantile == 0.95,-ncol(result)]
psm.med <- result[result$quantile == 0.5,-ncol(result)]
psm.mn <- d2p %>% 
  group_by(year) %>%
  reframe("psmhi" = mean(psmhi, na.rm = T))

f1 <- predict(loess(psm.max$psmhi ~ psm.max$year, span = 0.5))
f2 <- predict(loess(psm.mn$psmhi ~ psm.mn$year, span = 0.5))
f3 <- predict(loess(psm.min$psmhi ~ psm.min$year, span = 0.5))

st.mn <- d2p %>% 
  group_by(year) %>%
  reframe("ST.metric" = mean(ST.metric, na.rm = T))
psm <- cbind(st.mn, psm.mn[,2], psm.min[,2], psm.med[,2], psm.max[,2])
colnames(psm) <- c("year", "ST", "Mean", "Min", "Med", "Max")

plot1 <- psm |> ggplot() +
  geom_point(aes(x = ST, y = Mean, color = year), size = 3, alpha = 0.5)+
  geom_point(aes(x = ST, y = Max, color = year), size = 3, alpha = 0.5, shape = 17)+
  geom_point(aes(x = ST, y = Min, color = year), size = 3, alpha = 0.5, shape = 17)+
  geom_smooth(aes(x = ST, y = Mean), method = "loess", se = TRUE, color = "gray50") +
  geom_smooth(aes(x = ST, y = Max), method = "loess", se = TRUE, color = "gray50") +
  geom_smooth(aes(x = ST, y = Min), method = "loess", se = TRUE, color = "gray50") +
  scale_color_gradient(low = "blue", high = "red")+
  theme(axis.text = element_text(size = 12))+
  theme(axis.title = element_text(size = 12))+
  theme(axis.title.y = element_text(vjust = 3.5))+
  theme(axis.title.x = element_text(vjust = -1))+
  labs(x = "Mean August stream temperature (°C)")+
  labs(y = "Predicted pre-spawn mortality for wild Chinook salmon")+
  scale_x_continuous(limits = c(10, 20))+
  scale_y_continuous(limits = c(0, 1))

ggsave(filename = paste0("plots/", huc, "_", metric, "_PSM.png"), plot1, height = 4.25, width = 6)


plot1 <- psm |> ggplot() +
  geom_point(aes(x = year, y = Mean, color = ST), size = 2, alpha = 0.5)+
  geom_point(aes(x = year, y = Max, color = ST), size = 2, alpha = 0.5, shape = 17)+
  geom_point(aes(x = year, y = Min, color = ST), size = 2, alpha = 0.5, shape = 15)+
  geom_smooth(aes(x = year, y = Mean), method = "loess", se = TRUE, color = "gray50") +
  geom_smooth(aes(x = year, y = Max), method = "loess", se = TRUE, color = "gray50") +
  geom_smooth(aes(x = year, y = Min), method = "loess", se = TRUE, color = "gray50") +
  scale_color_gradient(low = "blue", high = "red")+
  theme(axis.text = element_text(size = 12))+
  theme(axis.title = element_text(size = 12))+
  theme(axis.title.y = element_text(vjust = 3.5))+
  theme(axis.title.x = element_text(vjust = -1))+
  labs(x = "Year")+
  labs(y = "Predicted pre-spawn mortality for wild Chinook salmon")+
  #scale_x_continuous(limits = c(10, 20))+
  scale_y_continuous(limits = c(0, 1))

ggsave(filename = paste0("plots/", huc, "_", metric, "_PSM_2.png"), plot1, height = 4.25, width = 6)


# by Origin
ggplot(NewData, aes(x = ST.metric, y = PSMPred, color = rev(as.factor(Origin)), border = rev(as.factor(Origin)), alpha = 0.1), size = 0.1)+
  geom_point()+
  theme(legend.position = "none")

# Alternative format Figure 7A ----
png("plots/PSM_over_time.png", width = 6, height = 6, units = "in", res = 300)

plot(psm.max$year, psm.max$psmhi, cex = .3, type = "n",
     las = 1, ylab = "Predicted pre-spawn mortality for wild Chinook salmon", xlab = "Year", ylim = c(0,1))
#polygon(c(psm.max$year, rev(psm.max$year)), c(rep(0,100), rev(f3)), col = "#0C7BDC80", border = NA)
#polygon(c(psm.max$year, rev(psm.max$year)), c(rep(0,100), rev(f2)), col = "#0C7BDC80", border = NA)
#polygon(c(psm.max$year, rev(psm.max$year)), c(rep(0,100), rev(f1)), col = "#0C7BDC80", border = NA)
lines(psm.max$year, f3, lwd = 2, col = "#0965B5")
lines(psm.max$year, f2, lwd = 2, col = "#0C7BDC")
lines(psm.max$year, f1, lwd = 2, col = "#A9C9E8")
points(psm.max$year, psm.min$psmhi, pch = 25, cex = 0.5, bg = "#0965B5", col = "#0965B5")
points(psm.max$year, psm.mn$psmhi, pch = 21, , cex = 0.5, bg = "#0C7BDC", col = "#0C7BDC")
points(psm.max$year, psm.max$psmhi, pch = 24, cex = 0.5, bg = "#A9C9E8", col = "#0C7BDC")

dev.off()
