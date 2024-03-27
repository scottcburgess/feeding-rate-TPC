### This code produces Figure 3 in the manuscript: 
### Powell, JA and  SC Burgess. How modularity and heterotrophy complicate the understanding of the causes of thermal performance curves: the case of feeding rate in a filter feeding animal
# Code finalized Mar 2024
# Any comments or error reporting, please contact Jackson Powell. jacksonpowell129@gmail.com

# R Version 4.3.1 (2023-06-16 ucrt) -- "Beagle Scouts"

# Load require libraries
library(dplyr) # Version 1.1.3. Needed for pipes
library(glmmTMB) # Version 1.1.8. Needed for glmmTMB()
library(AICcmodavg) # Version 2.3-2. Needed for aictab()

# Import data
# Set.wd() # set working directory, or file path, before importing data
dat <- read.csv("Clearance Rate Data.csv")
phenos <- read.csv("Phenotypic Data.csv")

# Subset for bowls with B. neritina present
bug <- dat[dat$bugula.present == "Y", ]

# Subset data for final data values
bug <- bug[bug$time == 2,]


# Change 'batch' to a factor
bug$batch <- factor(bug$batch)

# Use match() to add columns for number of zooids in each state and total number of zooids 
bug$feeding.zooids <- phenos[match(bug$bowl.name, phenos$name),
                          which(names(phenos) == "feeding.zooids")]
bug$regressed.zooids <- phenos[match(bug$bowl.name, phenos$name),
                               which(names(phenos) == "regressed.zooids")]
bug$dead.zooids <- phenos[match(bug$bowl.name, phenos$name),
                          which(names(phenos) == "dead.zooids")]
bug$total.zooids <- with(bug, feeding.zooids + regressed.zooids + dead.zooids)

# Mean of colony size 
size.mean <- mean(bug$total.zooids)

# Confidence intervals of colony size
size.se <- sd(bug$total.zooids)/sqrt(length(bug$total.zooids))

z_score <- 2 # Z-score for the 95% confidence interval

size.lwr <- size.mean - z_score * size.se
size.upr <- size.mean + z_score * size.se

# Use aggregate() to create data frames of mean values for total number of zooids and zooids of a given status 
t.z <- aggregate(total.zooids ~ realized.temp + batch, data = bug, FUN = mean)
f.z <- aggregate(cbind(feeding.zooids, total.zooids) ~ realized.temp + batch, data = bug, FUN = mean)
r.z <- aggregate(cbind(regressed.zooids, total.zooids) ~ realized.temp + batch, data = bug, FUN = mean)
d.z <- aggregate(cbind(dead.zooids, total.zooids) ~ realized.temp + batch, data = bug, FUN = mean)

# Use expand.grid() to create data frames to be filled with predicted values
p.b1 <- expand.grid(realized.temp = seq(min(bug[bug$batch == 1, ]$realized.temp),max(bug[bug$batch == 1, ]$realized.temp), by = 0.1), batch  = 1)
p.b2 <- expand.grid(realized.temp = seq(min(bug[bug$batch == 2, ]$realized.temp),max(bug[bug$batch == 2, ]$realized.temp), by = 0.1), batch  = 2)

# Arrange batches in order of increasing realized temperatures 
p.b1 <- p.b1 %>% arrange(realized.temp)
p.b2 <- p.b2 %>% arrange(realized.temp)

# Model selection and predictions

#a) Use glmmTMB() to model relationship between total zooid number and realized temperature with batch as a fixed effect
tm1 <- glmmTMB(round(total.zooids) ~ poly(realized.temp, 1) * batch, family = "nbinom1", data = t.z)
tm2 <- glmmTMB(round(total.zooids) ~ poly(realized.temp, 2) * batch, family = "nbinom1", data = t.z)
tm3 <- glmmTMB(round(total.zooids) ~ poly(realized.temp, 3) * batch, family = "nbinom1", data = t.z)
tm4 <- glmmTMB(round(total.zooids) ~ poly(realized.temp, 4) * batch, family = "nbinom1", data = t.z)

# Use aictab() to identify best fit model by AICc
aictab(list(tm1,tm2,tm3,tm4)) # Linear model is best fit

# Test for effects of batch and temperature based on tm1
tm1 <- glmmTMB(round(total.zooids) ~ realized.temp * batch, family = "nbinom1", data = t.z)
tm1a <- glmmTMB(round(total.zooids) ~ realized.temp + batch, family = "nbinom1", data = t.z)
tm1b <- glmmTMB(round(total.zooids) ~ realized.temp, family = "nbinom1", data = t.z)
tm1c <- glmmTMB(round(total.zooids) ~ batch, family = "nbinom1", data = t.z)

anova(tm1, tm1a) # No interactive effect between batch and temperature on colony size (χ2 = 2.981, df = 1, p = 0.148)
anova(tm1a, tm1b) # No difference in colony size between batches (χ2 = 0.12, df = 1, p = 0.729)
anova(tm1a, tm1c) # No difference in size of colonies placed into each temperature (χ2 = 2.186, df = 1, p = 0.139)

# Use predict() with best fit model to predict total zooid number as a function of temperature
p.t <- rbind(p.b1, p.b2)
preds.t <- predict(tm1a, newdata = p.t, se.fit = TRUE)
p.t$fit <- preds.t$fit
p.t$se.fit <- preds.t$se.fit

# Calculate and add confidence intervals to predicted data frame 
p.t$lwr <- p.t$fit - z_score * preds.t$se.fit
p.t$upr <- p.t$fit + z_score * preds.t$se.fit

# Use exp() to transform predicted values to count data
p.t$count <- exp(p.t$fit)
p.t$count.lwr <- exp(p.t$lwr)
p.t$count.upr <- exp(p.t$upr)

#b) Use glmmTMB() to model relationship between proportion of zooids capable of feeding within a colony and realized temperature with batch as a fixed effect

fm1 <- glmmTMB(cbind(round(feeding.zooids), round(total.zooids) - round(feeding.zooids)) ~ poly(realized.temp,1) * batch, family = "binomial", data = f.z)
fm2 <- glmmTMB(cbind(round(feeding.zooids), round(total.zooids) - round(feeding.zooids)) ~ poly(realized.temp,2) * batch, family = "binomial", data = f.z)
fm3 <- glmmTMB(cbind(round(feeding.zooids), round(total.zooids) - round(feeding.zooids)) ~ poly(realized.temp,3) * batch, family = "binomial", data = f.z)
fm4 <- glmmTMB(cbind(round(feeding.zooids), round(total.zooids) - round(feeding.zooids)) ~ poly(realized.temp,4) * batch, family = "binomial", data = f.z)

# Use aictab() to identify best fit model by AICc
aictab(list(fm1, fm2, fm3, fm4)) # Linear model is best fit 

# Test for effects of batch and temperature based on fm1
fm1 <- glmmTMB(cbind(round(feeding.zooids), round(total.zooids) - round(feeding.zooids)) ~ realized.temp * batch, family = "binomial", data = f.z)
fm1a <- glmmTMB(cbind(round(feeding.zooids), round(total.zooids) - round(feeding.zooids)) ~ realized.temp + batch, family = "binomial", data = f.z)
fm1b <- glmmTMB(cbind(round(feeding.zooids), round(total.zooids) - round(feeding.zooids)) ~ realized.temp, family = "binomial", data = f.z)
fm1c <- glmmTMB(cbind(round(feeding.zooids), round(total.zooids) - round(feeding.zooids)) ~ batch, family = "binomial", data = f.z)

anova(fm1, fm1a) # Significant interactive effect between batch and temperature on the proportion of feeding zooids (χ2 = 4.305, df = 1, p = 0.038)
anova(fm1, fm1b) # Significant interactive effect of batch on the proportion of feeding zooids (χ2 = 33.532, df = 2, p < 0.001)
anova(fm1, fm1c) # Significant effect of temperature on proportion of feeding zooids (χ2 = 39.408, df = 2, p < 0.001)
 
# Use confint() to compute confidence intervals for parameters for best fit model
confint(fm1)
# Proportion of feeding zooids decreased 0.035 (0.002 – 0.067, 95% CI) per°C faster in batch 1 than in batch 2

# Use predict() with best fit model to predict proportion of zooids capable of feeding as a function of temperature
p.f <- rbind(p.b1, p.b2)
preds.f <- predict(fm1, newdata = p.f, se.fit = TRUE)
p.f$fit <- preds.f$fit
p.f$se.fit <- preds.f$se.fit

# Calculate and add confidence intervals to predicted data frame 
p.f$lwr <- p.f$fit - z_score * preds.f$se.fit
p.f$upr <- p.f$fit + z_score * preds.f$se.fit

# Use plogis() to transform data to proportions
p.f$logis <- plogis(p.f$fit)
p.f$logis.lwr <- plogis(p.f$lwr)
p.f$logis.upr <- plogis(p.f$upr)

#c) Use glmmTMB() to model relationship between proportion of zooids regressed within a colony and realized temperature with batch as a fixed effect
rm1 <- glmmTMB(cbind(round(regressed.zooids), round(total.zooids) - round(regressed.zooids)) ~ poly(realized.temp,1) * batch, family = "binomial", data = r.z)
rm2 <- glmmTMB(cbind(round(regressed.zooids), round(total.zooids) - round(regressed.zooids)) ~ poly(realized.temp,2) * batch, family = "binomial", data = r.z)
rm3 <- glmmTMB(cbind(round(regressed.zooids), round(total.zooids) - round(regressed.zooids)) ~ poly(realized.temp,3) * batch, family = "binomial", data = r.z)
rm4 <- glmmTMB(cbind(round(regressed.zooids), round(total.zooids) - round(regressed.zooids)) ~ poly(realized.temp,4) * batch, family = "binomial", data = r.z)

# Use aictab() to identify best fit model by AICc
aictab(list(rm1, rm2, rm3, rm4)) # Linear model is best fit 

# Test for effects of batch and temperature based on rm1
rm1 <- glmmTMB(cbind(round(regressed.zooids), round(total.zooids) - round(regressed.zooids)) ~ realized.temp * batch, family = "binomial", data = r.z)
rm1a <- glmmTMB(cbind(round(regressed.zooids), round(total.zooids) - round(regressed.zooids)) ~ realized.temp + batch, family = "binomial", data = r.z)
rm1b <- glmmTMB(cbind(round(regressed.zooids), round(total.zooids) - round(regressed.zooids)) ~ realized.temp, family = "binomial", data = r.z)
rm1c <- glmmTMB(cbind(round(regressed.zooids), round(total.zooids) - round(regressed.zooids)) ~ batch, family = "binomial", data = r.z)

anova(rm1, rm1a) # Significant interactive effect between batch and temperature on proportion of regressed zooids (χ2 = 3.99, df = 1, p = 0.046)
anova(rm1, rm1b) # Significant effect of batch on proportion of regressed zooids (χ2 = 32.399, df = 2, p < 0.001)
anova(rm1, rm1c) # Significant effect of temperature on proportion of regressed zooids (χ2 = 38.621, df = 2, p < 0.001)

# Use confint() to compute confidence intervals for parameters for best fit model
confint(rm1)
# Proportion of regressed zooids increased 0.033 (0.001 – 0.066, 95% CI) per°C faster in batch 1 than batch 2

# Use predict() with best fit model to predict proportion of zooids regressed as a function of temperature
p.r <- rbind(p.b1, p.b2)
preds.r <- predict(rm1, newdata = p.r, se.fit = TRUE)
p.r$fit <- preds.r$fit
p.r$se.fit <- preds.r$se.fit

# Calculate and add confidence intervals to predicted data frame 
p.r$lwr <- p.r$fit - z_score * preds.r$se.fit
p.r$upr <- p.r$fit + z_score * preds.r$se.fit

# Use plogis() to transform data to proportions
p.r$logis <- plogis(p.r$fit)
p.r$logis.lwr <- plogis(p.r$lwr)
p.r$logis.upr <- plogis(p.r$upr)

#d) Use glmmTMB() to model relationship between proportion of zooids dead within a colony and realized temperature with batch as a fixed effect
dm1 <- glmmTMB(cbind(round(dead.zooids), round(total.zooids) - round(dead.zooids)) ~ poly(realized.temp,1) * batch, family = "binomial", data = d.z)
dm2 <- glmmTMB(cbind(round(dead.zooids), round(total.zooids) - round(dead.zooids)) ~ poly(realized.temp,2) * batch, family = "binomial", data = d.z)
dm3 <- glmmTMB(cbind(round(dead.zooids), round(total.zooids) - round(dead.zooids)) ~ poly(realized.temp,3) * batch, family = "binomial", data = d.z)
dm4 <- glmmTMB(cbind(round(dead.zooids), round(total.zooids) - round(dead.zooids)) ~ poly(realized.temp,4) * batch, family = "binomial", data = d.z)

# Only dm1 converged, so model selection using aictab() was omitted

# Test for effects of batch and temperature based on dm1
dm1 <- glmmTMB(cbind(round(dead.zooids), round(total.zooids) - round(dead.zooids)) ~ realized.temp * batch, family = "binomial", data = d.z)
dm1a <- glmmTMB(cbind(round(dead.zooids), round(total.zooids) - round(dead.zooids)) ~ realized.temp + batch, family = "binomial", data = d.z)
dm1b <- glmmTMB(cbind(round(dead.zooids), round(total.zooids) - round(dead.zooids)) ~ realized.temp , family = "binomial", data = d.z)
dm1c <- glmmTMB(cbind(round(dead.zooids), round(total.zooids) - round(dead.zooids)) ~ batch , family = "binomial", data = d.z)

anova(dm1, dm1a) # No interaction between temperature and batch (χ2 = 1.501, df = 1, p = 0.221)
anova(dm1a, dm1b) # Batches have the same intercept (χ2 = 0, df = 1, p = 0.998)
anova(dm1a, dm1c) # Temperature had a significant effect on the number of dead zooids (χ2 = 4.942, df = 1, p = 0.026)

# Use confint() to compute confidence intervals for parameters for best fit model
confint(dm1a)
# Proportion of dead zooids increased 0.285 (-0.008 – 0.578, 95% CI) per°C 

# Use predict() with best fit model to predict proportion of zooids dead as a function of temperature
p.d <- rbind(p.b1, p.b2)
preds.d <- predict(dm1a, newdata = p.d, se.fit = TRUE)
p.d$fit <- preds.d$fit
p.d$se.fit <- preds.d$se.fit

# Calculate and add confidence intervals to predicted data frame 
p.d$lwr <- p.d$fit - z_score * preds.d$se.fit
p.d$upr <- p.d$fit + z_score * preds.d$se.fit

# Use plogis() to transform data to proportions
p.d$logis <- plogis(p.d$fit)
p.d$logis.lwr <- plogis(p.d$lwr)
p.d$logis.upr <- plogis(p.d$upr)

# Make Figure 3

# Set up window 
windows(width = 4.5, height = 4.5) # use "quartz()" on Mac
par(mfrow = c(2, 2),mar=c(3, 5, 1, 1),oma=c(1, 0, 1, 0))

# Set up figure parameters 
cex <- 1
cex.axis <- 0.8
legend.cex <- 0.8
cex.lab <- 0.7
lwd <- 3
p.cex <- 1

# Set the desired number of tick marks for x-axis 
num_ticks <- 8

# Calculate tick positions
tick_positions <- seq(18, 32, length.out = num_ticks)

# Customize tick labels (e.g., formatting as integers)
tick_labels <- as.character(round(tick_positions))

# Set limits for axes
xlims <- c(min(p.t$realized.temp), max(p.t$realized.temp))
ylims <- c(0, 1) 

# a) Colony size (total zooid number)
with(bug, plot(1, type = "n", las = 1,bty = "l", xaxt = "n", xlab = "", ylab = "", xlim = xlims, ylim = c(min(total.zooids), max(total.zooids)), cex.axis = cex.axis))
axis(1, at = tick_positions, labels = rep("", num_ticks), cex.axis = cex.axis)
text(x = seq(tick_positions[1], tick_positions[8], by = 2),
     y = par("usr")[3] - ((par("usr")[4] - par("usr")[3]) * 0.075),
     labels  = tick_positions,
     xpd = NA,
     srt = 60,
     cex = cex.axis,
     adj = 1)
with(p.t[p.t$batch == 1, ], polygon(c(realized.temp, rev(realized.temp)), c(count.lwr, rev(count.upr)), col = adjustcolor("tomato", alpha = 0.4), border = "NA"))
with(p.t[p.t$batch == 2, ], polygon(c(realized.temp, rev(realized.temp)), c(count.lwr, rev(count.upr)), col = adjustcolor("dodgerblue", alpha = 0.4), border = "NA"))
with(t.z[t.z$batch == 1, ], points(realized.temp, total.zooids, pch  = 16, col = adjustcolor("tomato", alpha = 0.4), cex = p.cex))
with(t.z[t.z$batch == 2, ], points(realized.temp, total.zooids, pch  = 16, col = adjustcolor("dodgerblue", alpha = 0.4), cex = p.cex))
with(p.t[p.t$batch == 1, ], lines(realized.temp, count, lwd = lwd, col = "tomato"))
with(p.t[p.t$batch == 2, ], lines(realized.temp, count, lwd = lwd, col = "dodgerblue"))
mtext("Colony size \n(number of zooids)", side = 2, line = 2.5, adj = 0.5, cex = cex.lab, las = 3)
mtext(bquote(bold("A)")), side = 3, adj = .05, cex =  cex)
legend("topright", c("Batch 1","Batch 2"), bty="n", pch = rep(16, 2), cex = legend.cex, col = c("tomato", "dodgerblue"))

# b) Proportion of zooids capable of feeding
with(f.z, plot(1, type = "n", las = 1, bty = "l", xaxt = "n", xlab = "", ylab = "", xlim = xlims, ylim = ylims, cex.axis = cex.axis))
axis(1, at = tick_positions, labels = rep("", num_ticks), cex.axis = cex.axis)
text(x = seq(tick_positions[1], tick_positions[8], by = 2),
     y = par("usr")[3] - ((par("usr")[4] - par("usr")[3]) * 0.075),
     labels  = tick_positions,
     xpd = NA,
     srt = 60,
     cex = cex.axis,
     adj = 1)
with(p.f[p.f$batch == 1, ], polygon(c(realized.temp, rev(realized.temp)), c(logis.lwr, rev(logis.upr)), col = adjustcolor("tomato", alpha = 0.4), border = "NA"))
with(p.f[p.f$batch == 2, ], polygon(c(realized.temp, rev(realized.temp)), c(logis.lwr, rev(logis.upr)), col = adjustcolor("dodgerblue", alpha = 0.4), border = "NA"))
with(f.z[f.z$batch == 1, ], points(realized.temp, feeding.zooids/total.zooids, pch  = 16,col = adjustcolor("tomato", alpha = 0.4), cex = p.cex))
with(f.z[f.z$batch == 2, ], points(realized.temp, feeding.zooids/total.zooids, pch  = 16,col = adjustcolor("dodgerblue", alpha = 0.4), cex = p.cex))
with(p.f[p.f$batch == 1, ], lines(realized.temp, logis, lwd = lwd, col = "tomato"))
with(p.f[p.f$batch == 2, ], lines(realized.temp, logis, lwd = lwd, col = "dodgerblue"))
mtext("Proportion of \nfeeding zooids", side = 2, line = 2.5, adj = 0.5,cex = cex.lab, las = 3)
mtext(bquote(bold("B)")), side = 3, adj = .05, cex =  cex)
legend("topright", c("Batch 1", "Batch 2"), bty = "n", pch = rep(16, 2), cex = legend.cex, col = c("tomato", "dodgerblue"))

# c) Proportion of zooids regressed 
with(r.z, plot(1, type = "n", las = 1,bty = "l",xaxt = "n", xlab = "", ylab = "", xlim = xlims, ylim = ylims, cex.axis = cex.axis))
axis(1, at = tick_positions, labels = rep("", num_ticks), cex.axis = cex.axis)
text(x = seq(tick_positions[1], tick_positions[8], by = 2),
     y = par("usr")[3] - ((par("usr")[4] - par("usr")[3]) * 0.075),
     labels  = tick_positions,
     xpd = NA,
     srt = 60,
     cex = cex.axis,
     adj = 1)
with(p.r[p.r$batch == 1, ], polygon(c(realized.temp, rev(realized.temp)), c(logis.lwr,rev(logis.upr)), col = adjustcolor("tomato", alpha = 0.4), border = "NA"))
with(p.r[p.r$batch == 2, ], polygon(c(realized.temp, rev(realized.temp)), c(logis.lwr,rev(logis.upr)), col = adjustcolor("dodgerblue", alpha = 0.4), border = "NA"))
with(r.z[r.z$batch == 1, ], points(realized.temp, regressed.zooids/total.zooids, pch  = 16, col = adjustcolor("tomato", alpha = 0.4), cex = p.cex))
with(r.z[r.z$batch == 2, ], points(realized.temp, regressed.zooids/total.zooids, pch  = 16, col = adjustcolor("dodgerblue", alpha = 0.4), cex = p.cex))
with(p.r[p.r$batch == 1, ], lines(realized.temp, logis, lwd = lwd, col = "tomato"))
with(p.r[p.r$batch == 2, ], lines(realized.temp, logis, lwd = lwd, col = "dodgerblue"))
mtext("Proportion of \nregressed zooids", side = 2, line = 2.5, adj = 0.5, cex = cex.lab, las = 3)
mtext(bquote(bold("C)")), side = 3, adj = .05, cex =  cex)
legend("topleft", c("Batch 1", "Batch 2"), bty = "n", pch = rep(16, 2), cex = legend.cex, col = c("tomato", "dodgerblue"))

# d) Proportion of zooids dead
with(d.z, plot(1, type = "n", las = 1,bty = "l", xaxt = "n", xlab = "", ylab = "", xlim = xlims, ylim=c(0, 0.05), cex.axis = cex.axis))
axis(1, at = tick_positions, labels = rep("", num_ticks), cex.axis = cex.axis)
text(x = seq(tick_positions[1], tick_positions[8], by = 2),
     y = par("usr")[3] - ((par("usr")[4] - par("usr")[3]) * 0.075),
     labels = tick_positions,
     xpd = NA,
     srt = 60,
     cex = cex.axis,
     adj = 1)
with(p.d[p.d$batch == 1, ] ,polygon(c(realized.temp, rev(realized.temp)), c(logis.lwr, rev(logis.upr)), col = adjustcolor("tomato", alpha = 0.4), border = "NA"))
with(p.d[p.d$batch == 2, ] ,polygon(c(realized.temp, rev(realized.temp)), c(logis.lwr, rev(logis.upr)), col = adjustcolor("dodgerblue", alpha = 0.4), border = "NA"))
with(d.z[d.z$batch == 1, ], points(realized.temp, dead.zooids/total.zooids, pch  = 16,col = adjustcolor("tomato", alpha = 0.4), cex = p.cex))
with(d.z[d.z$batch == 2, ], points(realized.temp, dead.zooids/total.zooids, pch  = 16,col = adjustcolor("dodgerblue", alpha = 0.4), cex = p.cex))
with(p.d[p.d$batch == 1, ], lines(realized.temp, logis, lwd = lwd, col = "tomato"))
with(p.d[p.d$batch == 2, ], lines(realized.temp, logis, lwd =lwd, col = "dodgerblue"))
mtext("Proportion of \ndead zooids", side = 2, line = 3, adj = 0.5, cex = cex.lab, las = 3)
mtext(bquote(bold("D)")), side = 3, adj = .05, cex =  cex)
legend("topleft", c("Batch 1", "Batch 2"), bty = "n", pch = rep(16, 2), cex = legend.cex, col = c("tomato", "dodgerblue"))

# Add x-axis label
mtext(expression(paste("Temperature (",'\u00B0',"C)")), side = 1, line = -0.5, cex = cex.lab, outer = TRUE)


