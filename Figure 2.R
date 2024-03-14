### This code produces Figure 2 in the manuscript: 
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
phenos <- read.csv("Phenotypic Data.csv")

# Create a column of cells per milliliter from cells per microliter
dat$cells.per.ml <- dat$cells.per.ul * 1000

# Subset data by presence or absence of B. neritina
alg <- dat[dat$bugula.present == "N",]
bug <- dat[dat$bugula.present == "Y",]

# Create data frame for rate of change in algal characteristics to be added to
name_vec <- unique(alg$bowl.name)
alg.rates <- alg[alg$time == 0,] %>% select("bowl.name","tankID","target.temp","realized.temp","batch") 
alg.rates$r <- NA
V = 250 # Volume of water (ml) in each bowl 

# Calculate rate of change in algal concentration for bowls absent of B. neritina
for(i in 1:length(name_vec)){
  foo <- alg %>% filter(bowl.name==name_vec[i])
  start <- as.numeric(foo %>% filter(time==0) %>% select(cells.per.ml))
  end <- as.numeric(foo %>% filter(time==2) %>% select(cells.per.ml))
  r <- (log(end) - log(start)) / (2-0)
  alg.rates$r[i] <- r
  
}

# Use aggregate() to create data frame of mean values for rate of change in algal concentration
growth <- aggregate(r ~ realized.temp + batch, data = alg.rates, FUN = mean)

# Use glmmTMB() to model relationship between rate of change in algal concentration for both batches
lm1.b1 <- glmmTMB(r ~ realized.temp, data = growth[growth$batch == 1,])
lm1.b2 <- glmmTMB(r ~ realized.temp, data = growth[growth$batch == 2,])

# Use predict() with best fit model to predict rate of change in algal concentration as a function of temperature
p.b1 <- expand.grid(realized.temp = growth[growth$batch==1,]$realized.temp)
preds.b1 <- predict(lm1.b1, newdata = p.b1, se.fit = TRUE)
p.b1$fit <- preds.b1$fit
p.b1$se.fit <- preds.b1$se.fit
p.b1$batch <- rep(1, length = length(p.b1$fit))

p.b2 <- expand.grid(realized.temp = growth[growth$batch==2,]$realized.temp)
preds.b2 <- predict(lm1.b2, newdata = p.b2, se.fit = TRUE)
p.b2$fit <- preds.b2$fit
p.b2$se.fit <- preds.b2$se.fit
p.b2$batch <- rep(2, length = length(p.b2$fit))

# Calculate and add confidence intervals to predicted data frames 
z_score <- 2 # Z-score for the 95% confidence interval

p.b1$lwr <- p.b1$fit - z_score * preds.b1$se.fit
p.b1$upr <- p.b1$fit + z_score * preds.b1$se.fit

p.b2$lwr <- p.b2$fit - z_score * preds.b2$se.fit
p.b2$upr <- p.b2$fit + z_score * preds.b2$se.fit

# Combine data frames of predictions for both batches 
p <- rbind.data.frame(p.b1,p.b2)

# Calculate rate of change in algal concentration for bowls with B. neritina
name_vec <- unique(bug$bowl.name)
bug.rates <- bug[bug$time == 2,] %>% select("bowl.name","tankID","target.temp","realized.temp","batch")
bug.rates$r <- NA

for(i in 1:length(name_vec)){
  foo <- bug %>% filter(bowl.name==name_vec[i])
  start <- as.numeric(foo %>% filter(time==0) %>% select(cells.per.ml))
  end <- as.numeric(foo %>% filter(time==2) %>% select(cells.per.ml))
  r <- (log(end) - log(start)) / (2-0) # Equation 3  
  bug.rates$r[i] <- r
  
}

# Add predicted algal change to data frame with B. neritina bowl changes 
bug.rates$pred.alg.r <- p[match(round(bug.rates$realized.temp,2),round(p$realized.temp,2)),which(names(p) == "fit")]

# Use match() to add columns for number of zooids in each state and total number of zooids 
bug.rates$feeding.zooids <- phenos[match(bug.rates$bowl.name, phenos$name),
                                which(names(phenos) == "feeding.zooids")]
bug.rates$regressed.zooids <- phenos[match(bug.rates$bowl.name, phenos$name),
                                     which(names(phenos) == "regressed.zooids")]
bug.rates$dead.zooids <- phenos[match(bug.rates$bowl.name, phenos$name),
                                which(names(phenos) == "dead.zooids")]
bug.rates$total.zooids <- with(bug.rates, feeding.zooids + regressed.zooids + dead.zooids)

# Calculate clearance rates and add to data frame
bug.rates$F <- (bug.rates$r - bug.rates$pred.alg.r) * -1 # Multiply by -1 to put into units of B. neritina consumption
bug.rates$feeding.rate.per.colony <- (V/1)*bug.rates$F # Equation 4
bug.rates$feeding.rate.per.zooid <- (V/bug.rates$total.zooids)*bug.rates$F # Equation 5 (all zooids)
bug.rates$feeding.rate.per.feedingzooid <- (V/bug.rates$feeding.zooids)*bug.rates$F # Equation 5 (feeding zooids)

# Use aggregate() to create data frames of mean values for colony, zooid, and feeding zooid level clearance rates 
fr.c <- aggregate(feeding.rate.per.colony ~ realized.temp + batch, data = bug.rates, FUN = mean)
fr.z <- aggregate(feeding.rate.per.zooid ~ realized.temp + batch, data = bug.rates, FUN = mean)
fr.f <- aggregate(feeding.rate.per.feedingzooid ~ realized.temp + batch, data = bug.rates, FUN = mean)

# Model selection and predictions

# a) Use glmmTMB() to model relationship between colony level clearance rate and realized temperature with batch as a fixed effect
cm1 <- glmmTMB(feeding.rate.per.colony ~ poly(realized.temp,1) * batch, data = fr.c)
cm2 <- glmmTMB(feeding.rate.per.colony ~ poly(realized.temp,2) * batch, data = fr.c)
cm3 <- glmmTMB(feeding.rate.per.colony ~ poly(realized.temp,3) * batch, data = fr.c)
cm4 <- glmmTMB(feeding.rate.per.colony ~ poly(realized.temp,4) * batch, data = fr.c)

# Use aictab() to identify best fit model by AICc
aictab(list(cm1,cm2,cm3,cm4)) # linear model fits the data the best

# Test for effects of batch and temperature based on cm1
cm1 <- glmmTMB(feeding.rate.per.colony ~ realized.temp * batch, data = fr.c)
cm1a <- glmmTMB(feeding.rate.per.colony ~ realized.temp + batch, data = fr.c)
cm1b <- glmmTMB(feeding.rate.per.colony ~ realized.temp, data = fr.c)

anova(cm1,cm1a) # No interaction between temperature and batch on colony-level clearance rate (χ2 = 2.744, df = 1, p = 0.098)
anova(cm1a,cm1b) # Batches have different intercepts (χ2 = 6.633, df = 1, p = 0.01)
cm1c <- glmmTMB(feeding.rate.per.colony ~ 1 + batch, data = fr.c)
anova(cm1a,cm1c) # Significant effect of temperature on colony-level clearance rate (χ2 = 4.873,, df = 1, p < 0.027)

# Use confint() to compute confidence intervals for parameters for best fit model
confint(cm1a)
# The colony-level clearance rate increased by by 7.334 (1.312 – 13.356, 95% CI) cells cells-1 ml-1 day-1 for every °C increase in realized temperature
# The colony-level clearance rate was 87.068 (27.542 – 146.594, 95% CI) cells cells-1 ml-1 day-1 lower in batch 2 compared to batch 1.

# Use predict() with best fit model to predict colony-level clearance rate as a function of temperature
p.c <- rbind(p.b1,p.b2)
preds.c <- predict(cm1a, newdata = p.c, se.fit = TRUE)
p.c$fit <- preds.c$fit
p.c$se.fit <- preds.c$se.fit

# Calculate and add confidence intervals to predicted data frame 
p.c$lwr <- p.c$fit - z_score * preds.c$se.fit
p.c$upr <- p.c$fit + z_score * preds.c$se.fit

# b) Use glmmTMB() to model relationship between zooid level clearance rate and realized temperature with batch as a fixed effect
zm1 <- glmmTMB(feeding.rate.per.zooid ~ poly(realized.temp,1) * batch, data = fr.z)
zm2 <- glmmTMB(feeding.rate.per.zooid ~ poly(realized.temp,2) * batch, data = fr.z)
zm3 <- glmmTMB(feeding.rate.per.zooid ~ poly(realized.temp,3) * batch, data = fr.z)
zm4 <- glmmTMB(feeding.rate.per.zooid ~ poly(realized.temp,4) * batch, data = fr.z)

# Use aictab() to identify best fit model by AICc
aictab(list(zm1,zm2,zm3,zm4)) # Linear model fits the data best 

# Likelihood ratio tests based on zm1
zm1 <- glmmTMB(feeding.rate.per.zooid ~ realized.temp * batch, data = fr.z)
zm1a <- glmmTMB(feeding.rate.per.zooid ~ realized.temp + batch, data = fr.z)
zm1b <- glmmTMB(feeding.rate.per.zooid ~ realized.temp, data = fr.z)

anova(zm1,zm1a) # No interaction between temperature and batch (χ2 = 0.2067, df = 1, p = 0.649)
anova(zm1a,zm1b) # Batches have different intercepts (χ2 = 4.245, df = 1, p = 0.039)
zm1c <- glmmTMB(feeding.rate.per.zooid ~ 1 + batch, data = fr.z)
anova(zm1a,zm1c) # Significant effect of temperature on clearance rate per zooid (χ2 = 6.503, df = 1, p = 0.011)

# Use confint() to compute confidence intervals for parameters for best fit model
confint(zm1a)
# The clearance rate per zooid increased by by 0.063 (0.019 – 0.106, 95% CI) cells cells-1 ml-1 day-1 for every °C increase in realized temperature
# The clearance rate per zooid was 0.481 (0.053 – 0.909, 95% CI) cells cells-1 ml-1 day-1 lower in batch 2 compared to batch 1.

# Use predict() with best fit model to predict zooid level clearance rate as a function of temperaturep.z <- rbind(p.b1,p.b2)
p.z <- rbind(p.b1,p.b2)
preds.z <- predict(zm1a, newdata = p.z, se.fit = TRUE)
p.z$fit <- preds.z$fit
p.z$se.fit <- preds.z$se.fit

# Calculate and add confidence intervals to predicted data frame 
p.z$lwr <- p.z$fit - z_score * preds.z$se.fit
p.z$upr <- p.z$fit + z_score * preds.z$se.fit

# c) Use glmmTMB() to model relationship between feeding zooid level clearance rate and realized temperature with batch as a fixed effect
fm1 <- glmmTMB(feeding.rate.per.feedingzooid ~ poly(realized.temp,1) * batch, data = fr.f)
fm2 <- glmmTMB(feeding.rate.per.feedingzooid ~ poly(realized.temp,2) * batch, data = fr.f)
fm3 <- glmmTMB(feeding.rate.per.feedingzooid ~ poly(realized.temp,3) * batch, data = fr.f)
fm4 <- glmmTMB(feeding.rate.per.feedingzooid ~ poly(realized.temp,4) * batch, data = fr.f)

# Use aictab() to identify best fit model by AICc
aictab(list(fm1,fm2,fm3,fm4)) # Linear model fits data best 

# Test for effects of batch and temperature based on fm1
fm1 <- glmmTMB(feeding.rate.per.feedingzooid ~ realized.temp * batch, data = fr.f)
fm1a <- glmmTMB(feeding.rate.per.feedingzooid ~ realized.temp + batch, data = fr.f)
fm1b <- glmmTMB(feeding.rate.per.feedingzooid ~ realized.temp, data = fr.f)

anova(fm1,fm1a) # No interaction between temperature and batch (X2 = 0.0031, df = 1, p = .956)
anova(fm1a,fm1b) # No difference in intercept between batches (X2 = 0.921, df = 1, p = .337)
fm1c <- glmmTMB(feeding.rate.per.feedingzooid ~ 1, data = fr.f)
anova(fm1b,fm1c) # Significant effect of temperature on clearance rate per feeding zooid (X2 = 9.395, df = 1, p = .002)

# Use confint() to compute confidence intervals for parameters for best fit model
confint(fm1a)
# The clearance rate per feeding zooid increased by by 0.324 (0.152 – 0.495, 95% CI) cells cells-1 ml-1 day-1 for every °C increase in realized temperature
# The clearance rate per feeding zooid was 0.8423 (2.538 –  - 0.853, 95% CI) cells cells-1 ml-1 day-1 lower in batch 2 compared to batch 1.

# Use predict() with best fit model to predict feeding zooid level clearance rate as a function of temperature
p.f <- rbind(p.b1,p.b2)
preds.f <- predict(fm1a, newdata = p.f, se.fit = TRUE)
p.f$fit <- preds.f$fit
p.f$se.fit <- preds.f$se.fit

# Calculate and add confidence intervals to predicted data frame 
p.f$lwr <- p.f$fit - z_score * preds.f$se.fit
p.f$upr <- p.f$fit + z_score * preds.f$se.fit

# Make Figure 2

# Set up window
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

# Set limits for axes
xlims <- c(min(bug.rates$realized.temp), max(bug.rates$realized.temp))

# a) Colony level clearance rate
with(fr.c, plot(1, type="n", las = 1,bty = "l",  xaxt = "n", xlab="", ylab = "", 
                xlim=xlims, ylim=c(min(p.c$lwr),max(fr.c$feeding.rate.per.colony)), cex.axis = cex.axis, panel.first = abline(h = 0, lty = 2, col = "darkgray")))
axis(1, at = tick_positions, labels = rep("",num_ticks), cex.axis = cex.axis)
text(x = seq(tick_positions[1],tick_positions[8],by =2),
     y = par("usr")[3] - ((par("usr")[4]-par("usr")[3]) *0.05),
     labels  = tick_positions,
     xpd =NA,
     srt = 60,
     cex = cex.axis,
     adj = 1)
with(p.c[p.c$batch == 1,],polygon(c(realized.temp,rev(realized.temp)), c(lwr,rev(upr)), col = adjustcolor("tomato", alpha = .4), border = NA))
with(p.c[p.c$batch == 2,],polygon(c(realized.temp,rev(realized.temp)), c(lwr,rev(upr)), col = adjustcolor("dodgerblue", alpha = .4), border = NA))
with(fr.c[fr.c$batch == 1,], points(realized.temp,feeding.rate.per.colony, pch  = 16, cex = p.cex, col = adjustcolor("tomato", alpha = .4)))
with(fr.c[fr.c$batch == 2,], points(realized.temp,feeding.rate.per.colony, pch  = 16, cex = p.cex, col = adjustcolor("dodgerblue", alpha = .4)))
with(p.c[p.c$batch == 1,], lines(realized.temp,fit, lwd = lwd, col = "tomato"))
with(p.c[p.c$batch == 2,], lines(realized.temp,fit, lwd =lwd, col = "dodgerblue"))
mtext("Clearance rate per colony", side = 2, line = 5, adj=0.5,cex=cex, las = 3)
mtext(expression(paste("(ml"," ", "day"^"-1", "colony"^"-1",")")), side = 2, line = 3, adj=0.5,cex=cex, las = 3)
mtext("a)", side = 3, adj = 0.05, cex =  cex)
legend("topleft", c("Batch 1","Batch 2"), bty="n", pch = rep(16, 2), cex= legend.cex, col = c("tomato","dodgerblue"))

# b) Per zooid clearance rate
with(fr.z, plot(1, type="n", las = 1,bty = "l",  xaxt = "n", xlab="", ylab = "",
                xlim=xlims, ylim=c(min(p.f$lwr),max(fr.f$feeding.rate.per.feedingzooid)), cex.axis = cex.axis, panel.first = abline(h = 0, lty = 2, col = "darkgray")))
axis(1, at = tick_positions, labels = rep("",num_ticks), cex.axis = cex.axis)
text(x = seq(tick_positions[1],tick_positions[8],by =2),
     y = par("usr")[3] - ((par("usr")[4]-par("usr")[3]) *0.05),
     labels  = tick_positions,
     xpd =NA,
     srt = 60,
     cex = cex.axis,
     adj = 1)
with(p.z[p.z$batch == 1,],polygon(c(realized.temp,rev(realized.temp)), c(lwr,rev(upr)), col = adjustcolor("tomato", alpha = .4), border = NA))
with(p.z[p.z$batch == 2,],polygon(c(realized.temp,rev(realized.temp)), c(lwr,rev(upr)), col = adjustcolor("dodgerblue", alpha = .4), border = NA))
with(fr.z[fr.z$batch == 1,], points(realized.temp,feeding.rate.per.zooid, pch  = 16, cex = p.cex, col = adjustcolor("tomato", alpha = .4)))
with(fr.z[fr.z$batch == 2,], points(realized.temp,feeding.rate.per.zooid, pch  = 16, cex = p.cex, col = adjustcolor("dodgerblue", alpha = .4)))
with(p.z[p.z$batch == 1,], lines(realized.temp,fit, lwd = lwd, col = "tomato"))
with(p.z[p.z$batch == 2,], lines(realized.temp,fit, lwd =lwd, col = "dodgerblue"))
mtext("Clearance rate per zooid", side = 2, line = 5, adj=0.5,cex=cex, las = 3)
mtext(expression(paste(" (ml"," ", "day"^"-1", "zooid"^"-1",")")), side = 2, line = 3, adj=0.5,cex=cex, las = 3)
mtext("b)", side = 3, adj = 0.05, cex =  cex)
legend("topleft", c("Batch 1","Batch 2"), bty="n", pch = rep(16, 2), cex= legend.cex, col = c("tomato","dodgerblue"))

# c) Per feeding zooid clearance rate
with(fr.f, plot(1, type="n", las = 1,bty = "l",  xaxt = "n", xlab="", ylab = "",
                xlim = xlims, ylim=c(min(p.f$lwr),max(fr.f$feeding.rate.per.feedingzooid)), cex.axis = cex.axis, panel.first = abline(h = 0, lty = 2, col = "darkgray")))
axis(1, at = tick_positions, labels = rep("",num_ticks), cex.axis = cex.axis)
text(x = seq(tick_positions[1],tick_positions[8],by =2),
     y = par("usr")[3] - ((par("usr")[4]-par("usr")[3]) *0.05),
     labels  = tick_positions,
     xpd =NA,
     srt = 60,
     cex = cex.axis,
     adj = 1)
with(p.f[p.f$batch == 1,],polygon(c(realized.temp,rev(realized.temp)), c(lwr,rev(upr)), col = adjustcolor("tomato", alpha = .4), border = NA))
with(p.f[p.f$batch == 2,],polygon(c(realized.temp,rev(realized.temp)), c(lwr,rev(upr)), col = adjustcolor("dodgerblue", alpha = .4), border = NA))
with(fr.f[fr.f$batch == 1,], points(realized.temp,feeding.rate.per.feedingzooid, pch  = 16, cex = p.cex, col = adjustcolor("tomato", alpha = .4)))
with(fr.f[fr.f$batch == 2,], points(realized.temp,feeding.rate.per.feedingzooid, pch  = 16, cex = p.cex, col = adjustcolor("dodgerblue", alpha = .4)))
with(p.f[p.f$batch == 1,], lines(realized.temp,fit, lwd = lwd, col = "tomato"))
with(p.f[p.f$batch == 2,], lines(realized.temp,fit, lwd =lwd, col = "dodgerblue"))
mtext("Clearance rate", side = 2, line = 6.5, cex=cex, las = 3)
mtext("per feeding zooid", side = 2, line = 4.5, cex=cex, las = 3)
mtext(expression(paste(" (ml"," ", "day"^"-1", "feeding zooid"^"-1",")")), side = 2, line = 2.5, adj=0.5,cex=cex, las = 3)

mtext("c)", side = 3, adj = 0.05, cex =  cex)
legend("topleft", c("Batch 1","Batch 2"), bty="n", pch = rep(16, 2), cex= legend.cex, col = c("tomato","dodgerblue"))

# Add x-axis label
mtext(expression(paste("Temperature (",'\u00B0',"C)")), side = 1, line = -0.5, cex = cex, outer = TRUE)
