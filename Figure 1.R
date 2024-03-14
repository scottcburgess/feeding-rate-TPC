### This code produces Figure 1 in the manuscript: 
### Powell, JA and  SC Burgess. How modularity and heterotrophy complicate the understanding of the causes of thermal performance curves: the case of feeding rate in a filter feeding animal
# Code finalized Mar 2024
# Any comments or error reporting, please contact Jackson Powell. jap16f@fsu.edu

# R Version 4.3.1 (2023-06-16 ucrt) -- "Beagle Scouts"

# Load require libraries
library(dplyr) # Version 1.1.3. Needed for pipes
library(glmmTMB) # Version 1.1.8. Needed for glmmTMB()
library(AICcmodavg) # Version 2.3-2. Needed for aictab()

# Import data
# Set.wd() # set working directory, or file path, before importing data
dat <- read.csv("Clearance Rate Data.csv")

# Create a column of cells per milliliter from cells per microliter
dat$cells.per.ml <- dat$cells.per.ul * 1000

# Subset data by absence of B. neritina
alg <- dat[dat$bugula.present == "N",]

# Create data frame for rate of change in algal characteristics to be added to
name_vec <- unique(alg$bowl.name)
alg.rates <- alg[alg$time == 0,] %>% select("bowl.name","tankID","target.temp","realized.temp","batch") 
alg.rates$r <- NA

# Calculate rate of change in algal concentration for bowls absent of B. neritina
for(i in 1:length(name_vec)){
  foo <- alg %>% filter(bowl.name==name_vec[i])
  start <- as.numeric(foo %>% filter(time==0) %>% select(cells.per.ml))
  end <- as.numeric(foo %>% filter(time==2) %>% select(cells.per.ml))
  r <- (log(end) - log(start)) / (2-0) # equation 3
  alg.rates$r[i] <- r
  
}

# Calculate rate of change in proxy for algal cell size (forward scatter area) for bowls absent of B. neritina
for(i in 1:length(name_vec)){
  foo <- alg %>% filter(bowl.name==name_vec[i])
  start <- as.numeric(foo %>% filter(time==0) %>% select(fsc))
  end <- as.numeric(foo %>% filter(time==2) %>% select(fsc))
  diff <- ((end) - (start)) / (2-0) 
  alg.rates$fsc.diff[i] <- diff
  
}

# Calculate rate of change in proxy for algal cell internal complexity (side scatter area) for bowls absent of B. neritina
for(i in 1:length(name_vec)){
  foo <- alg %>% filter(bowl.name==name_vec[i])
  start <- as.numeric(foo %>% filter(time==0) %>% select(ssc))
  end <- as.numeric(foo %>% filter(time==2) %>% select(ssc))
  diff <- ((end) - (start)) / (2-0)
  alg.rates$ssc.diff[i] <- diff
  
}

# Use aggregate() to create data frames of mean values per water bath (=realized.temp) for rate of change in algal cell concentration, size, and internal complexity
growth <- aggregate(r ~ realized.temp + batch, data = alg.rates, FUN = mean)
fsc.change <- aggregate(fsc.diff ~ realized.temp + batch, data = alg.rates, FUN = mean)
ssc.change <- aggregate(ssc.diff ~ realized.temp + batch, data = alg.rates, FUN = mean)

# Model selection and predictions

# a) Use glmmTMB() to model relationship between rate of change for algal concentration and realized temperature with batch as a fixed effect
lm1 <- glmmTMB(r ~ poly(realized.temp,1) * batch, data = growth)
lm2 <- glmmTMB(r ~ poly(realized.temp,2) * batch, data = growth)
lm3 <- glmmTMB(r ~ poly(realized.temp,3) * batch, data = growth)
lm4 <- glmmTMB(r ~ poly(realized.temp,4) * batch, data = growth)

# Use aictab() to identify best fit model by AICc
aictab(list(lm1,lm2,lm3,lm4)) # linear model fits the data best

# Test for effects of batch and temperature, using lm1
lm1 <- glmmTMB(r ~ realized.temp * batch, data = growth)
lm1a <- glmmTMB(r ~ realized.temp + batch, data = growth)
lm1b <- glmmTMB(r ~ realized.temp, data = growth)

anova(lm1, lm1a) # No interaction between temperature and batch (χ2 = 1.178, df = 1, p = 0.278)
anova(lm1a, lm1b) # Batches have different intercepts (χ2 = 5.193, df = 1, p = 0.023)
lm1c <- glmmTMB(r ~ 1 + batch, data  = growth)
anova(lm1a, lm1c) # Significant effect of temperature on rate of change in cell concentration (χ2 = 4.839, df = 1, p = 0.028)

# Use confint() to compute confidence intervals for parameters for best fit model
confint(lm1a)
# The rate of change in algal concentration increased by 0.008 (0.001 – 0.014, 95% CI) cells cells-1 day-1 for every °C increase in realized temperature
# The rate of change in algal concentration was 0.077 (0.016 – 0.139, 95% CI) cells cells-1 ml-1 day-1 lower in batch 2 compared to batch 1.

# Use expand.grid() to create data frames to be filled with predicted values
p.b1 <- expand.grid(realized.temp = seq(min(alg[alg$batch==1,]$realized.temp),max(alg[alg$batch==1,]$realized.temp), by =0.1), batch  = 1)
p.b2 <- expand.grid(realized.temp = seq(min(alg[alg$batch==2,]$realized.temp),max(alg[alg$batch==2,]$realized.temp), by =0.1), batch  = 2)

# Arrange batches in order of increasing realized temperatures 
p.b1 <- p.b1 %>% arrange(realized.temp)
p.b2 <- p.b2 %>% arrange(realized.temp)

# Use predict function with model to predict rate of change in cell concentration as a function of temperature, including standard error
p <- rbind(p.b1,p.b2)
preds <- predict(lm1a, newdata = p, se.fit = TRUE)
p$fit <- preds$fit
p$se.fit <- preds$se.fit

# Calculate and add confidence intervals to predicted data frame 
z_score <- 2 # Z-score for the 95% confidence interval
p$lwr <- p$fit - z_score * preds$se.fit
p$upr <- p$fit + z_score * preds$se.fit


# b) Use glmmTMB() to model relationship between rate of change for algal cell size and realized temperature with batch as a fixed effect
fm1 <- glmmTMB(fsc.diff ~ poly(realized.temp,1) * batch, data = fsc.change)
fm2 <- glmmTMB(fsc.diff ~ poly(realized.temp,2) * batch, data = fsc.change)
fm3 <- glmmTMB(fsc.diff ~ poly(realized.temp,3) * batch, data = fsc.change)
fm4 <- glmmTMB(fsc.diff ~ poly(realized.temp,4) * batch, data = fsc.change)

# Use aictab() to identify best fit model by AICc
aictab(list(fm1,fm2,fm3,fm4)) # linear model fits the data best

# Test for effects of batch and temperature using fm1
fm1 <- glmmTMB(fsc.diff ~ realized.temp * batch, data = fsc.change)
fm1a <- glmmTMB(fsc.diff ~ realized.temp + batch, data = fsc.change)
fm1b <- glmmTMB(fsc.diff ~ realized.temp, data = fsc.change)

anova(fm1, fm1a) # No interaction between temperature and batch (χ2 = 0.5017, df = 1, p = 0.479)
anova(fm1a, fm1b) # Batches have the same intercept (χ2 = 1.071, df = 1, p = 0.301)
fm1c <- glmmTMB(fsc.diff ~ 1, data  = fsc.change)
anova(fm1b, fm1c) # Significant effect of temperature on magnitude of change to FSC-A (χ2 = 8.821, df = 1, p = 0.003)

# Use confint() to compute confidence intervals for parameters for best fit model
confint(fm1a)
# The rate of change for algal cell size increased by 1045.472 (293.401 – 1797.544, 95% CI) FSC day-1 for every °C increase in realized temperature
# The rate of change for algal cell size was 7055.69 (14352.548 – - 241.167, 95% CI) FSC day-1 lower in batch 2 compared to batch 1.

# Use predict() with best fit model to predict algal cell size as a function of temperature
p.f <- rbind(p.b1,p.b2)
preds.f <- predict(fm1a, newdata = p.f, se.fit = TRUE)
p.f$fit <- preds.f$fit
p.f$se.fit <- preds.f$se.fit

# Calculate and add confidence intervals to predicted data frame 
p.f$lwr <- p.f$fit - z_score * preds.f$se.fit
p.f$upr <- p.f$fit + z_score * preds.f$se.fit

# b) Use glmmTMB() to model relationship between rate of change for internal complexity and realized temperature with batch as a fixed effect
sm1 <- glmmTMB(ssc.diff ~ poly(realized.temp,1) * batch, data = ssc.change)
sm2 <- glmmTMB(ssc.diff ~ poly(realized.temp,2) * batch, data = ssc.change)
sm3 <- glmmTMB(ssc.diff ~ poly(realized.temp,3) * batch, data = ssc.change)
sm4 <- glmmTMB(ssc.diff ~ poly(realized.temp,4) * batch, data = ssc.change)

# Use aictab() to identify best fit model by AICc
aictab(list(sm1,sm2,sm3,sm4)) # linear model fits the data best

# Test for effects of batch and temperature using sm1
sm1 <- glmmTMB(ssc.diff ~ realized.temp * batch, data = ssc.change)
sm1a <- glmmTMB(ssc.diff ~ realized.temp + batch, data = ssc.change)
sm1b <- glmmTMB(ssc.diff ~ realized.temp, data = ssc.change)

anova(sm1, sm1a) # No interaction between temperature and batch (χ2 = 0, df = 1, p = 1)
anova(sm1a, sm1b) # Batches have the same intercept (χ2 = 0.021, df = 1, p = 0.885)
sm1c <- glmmTMB(ssc.diff ~ 1, data  = ssc.change)
anova(sm1b, sm1c) # No significant effect of temperature on magnitude of change to SSC-A day-1 (χ2 = 2.431, df = 1, p = 0.119)

# Use predict() with best fit model to predict rate of change for algal cell internal complexity as a function of temperature
p.s <- rbind(p.b1,p.b2)
preds.s <- predict(sm1b, newdata = p.f, se.fit = TRUE)
p.s$fit <- preds.s$fit
p.s$se.fit <- preds.s$se.fit

# Calculate and add confidence intervals to predicted data frame 
p.s$lwr <- p.s$fit - z_score * preds.s$se.fit
p.s$upr <- p.s$fit + z_score * preds.s$se.fit

# Make Figure 1

# Set up Window 
windows(width=9,height=3) # use "quartz()" on Mac
par(mfrow = c(1,3),mar=c(5,8,1,1),oma=c(1,0,1,0))

# Set up figure parameters
cex <- 1.2
cex.axis <- 1.4
legend.cex <- 1.4
lwd <- 3
p.cex <- 1.3

# Set the desired number of tick marks for x-axis 
num_ticks <- 8

# Calculate tick positions
tick_positions <- seq(18, 32, length.out = num_ticks)

# Customize tick labels (e.g., formatting as integers)
tick_labels <- as.character(round(tick_positions))

# a) Rate of change in algal cell concentration
plot(1, type="n", las = 1,bty = "l", xaxt = "n", xlab="", ylab = "", xlim=c(16,34), ylim=c(min(alg.rates$r),max(alg.rates$r)), cex.axis = cex.axis,  panel.first = abline(h = 0, lty = 2, col = "darkgray"))
axis(1, at = tick_positions, labels = rep("",num_ticks), cex.axis = cex.axis)
text(x = seq(tick_positions[1],tick_positions[8],by =2),
     y = par("usr")[3] - ((par("usr")[4]-par("usr")[3]) *0.05),
     labels  = tick_positions,
     xpd =NA,
     srt = 60,
     cex = cex.axis,
     adj = 1)
with(p[p$batch == 1,],polygon(c(realized.temp,rev(realized.temp)), c(lwr,rev(upr)), col = adjustcolor("tomato", alpha = .4), border = "NA"))
with(p[p$batch == 2,],polygon(c(realized.temp,rev(realized.temp)), c(lwr,rev(upr)), col = adjustcolor("dodgerblue", alpha = .4), border = "NA"))
with(growth[growth$batch == 1,], points(realized.temp,r, pch  = 16, col = adjustcolor("tomato", alpha = .4), cex = p.cex))
with(growth[growth$batch == 2,], points(realized.temp,r, pch  = 16,col = adjustcolor("dodgerblue", alpha = .4), cex = p.cex))
with(p[p$batch == 1,], lines(realized.temp,fit, lwd = lwd, col = "tomato"))
with(p[p$batch == 2,], lines(realized.temp,fit, lwd =lwd, col = "dodgerblue"))
mtext("Rate of change in \nalgal concentration", side = 2, line = 3, cex=cex, las = 3)
mtext("a)", side = 3, adj = 0.05, cex =  cex)
legend("topright", c("Batch 1","Batch 2"), bty="n", pch = rep(16, 2), cex= legend.cex, col = c("tomato","dodgerblue"))


# b) Rate of change in algal cell size
plot(1, type="n", las = 1,bty = "l", xaxt = "n", xlab="", ylab = "", xlim=c(16,34), ylim=c(min(alg.rates$fsc.diff),max(alg.rates$fsc.diff)), cex.axis = cex.axis,  panel.first = abline(h = 0, lty = 2, col = "darkgray"))
axis(1, at = tick_positions, labels = rep("",num_ticks), cex.axis = cex.axis)
text(x = seq(tick_positions[1],tick_positions[8],by =2),
     y = par("usr")[3] - ((par("usr")[4]-par("usr")[3]) *0.05),
     labels  = tick_positions,
     xpd =NA,
     srt = 60,
     cex = cex.axis,
     adj = 1)
with(p.f[p.f$batch == 1,],polygon(c(realized.temp,rev(realized.temp)), c(lwr,rev(upr)), col = adjustcolor("tomato", alpha = .4), border = "NA"))
with(p.f[p.f$batch == 2,],polygon(c(realized.temp,rev(realized.temp)), c(lwr,rev(upr)), col = adjustcolor("dodgerblue", alpha = .4), border = "NA"))
with(fsc.change[fsc.change$batch == 1,], points(realized.temp,fsc.diff, pch  = 16, col = adjustcolor("tomato", alpha = .4), cex = p.cex))
with(fsc.change[fsc.change$batch == 2,], points(realized.temp,fsc.diff, pch  = 16,col = adjustcolor("dodgerblue", alpha = .4), cex = p.cex))
with(p.f[p.f$batch == 1,], lines(realized.temp,fit, lwd = lwd, col = "tomato"))
with(p.f[p.f$batch == 2,], lines(realized.temp,fit, lwd =lwd, col = "dodgerblue"))
mtext("Rate of change in size", side = 2, line = 4, cex=cex, las = 3)
mtext("b)", side = 3, adj = 0.05, cex =  cex)
legend("topleft", c("Batch 1","Batch 2"), bty="n", pch = rep(16, 2), cex= legend.cex, col = c("tomato","dodgerblue"))

# c)  Rate of change in algal cell internal complexity
plot(1, type="n", las = 1,bty = "l", xaxt = "n", xlab="", ylab = "", xlim=c(16,34), ylim=c(min(alg.rates$ssc.diff),max(alg.rates$ssc.diff)), cex.axis = cex.axis,  panel.first = abline(h = 0, lty = 2, col = "darkgray"))
axis(1, at = tick_positions, labels = rep("",num_ticks), cex.axis = cex.axis)
text(x = seq(tick_positions[1],tick_positions[8],by =2),
     y = par("usr")[3] - ((par("usr")[4]-par("usr")[3]) *0.05),
     labels  = tick_positions,
     xpd =NA,
     srt = 60,
     cex = cex.axis,
     adj = 1)

with(p.s[p.s$batch == 1,],polygon(c(realized.temp,rev(realized.temp)), c(lwr,rev(upr)), col = adjustcolor("tomato", alpha = .4), border = "NA"))
with(p.s[p.s$batch == 2,],polygon(c(realized.temp,rev(realized.temp)), c(lwr,rev(upr)), col = adjustcolor("dodgerblue", alpha = .4), border = "NA"))
with(ssc.change[ssc.change$batch == 1,], points(realized.temp,ssc.diff, pch  = 16, col = adjustcolor("tomato", alpha = .4), cex = p.cex))
with(ssc.change[ssc.change$batch == 2,], points(realized.temp,ssc.diff, pch  = 16,col = adjustcolor("dodgerblue", alpha = .4), cex = p.cex))
with(p.s[p.s$batch == 1,], lines(realized.temp,fit, lwd = lwd, col = "tomato"))
with(p.s[p.s$batch == 2,], lines(realized.temp,fit, lwd =lwd, col = "dodgerblue"))
mtext("Rate of change in \ninternal complexity", side = 2, line = 4, cex=cex, las = 3)
mtext("c)", side = 3, adj = 0.05, cex =  cex)
legend("topleft", c("Batch 1","Batch 2"), bty="n", pch = rep(16, 2), cex= legend.cex, col = c("tomato","dodgerblue"))

# X-axis title
mtext(expression(paste("Temperature (",'\u00B0',"C)")), side = 1, line = -0.5, cex = cex, outer = TRUE)
