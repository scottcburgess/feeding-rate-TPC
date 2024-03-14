### This code produces Figure 4 in the manuscript: 
### Powell, JA and  SC Burgess. How modularity and heterotrophy complicate the understanding of the causes of thermal performance curves: the case of feeding rate in a filter feeding animal
# Code finalized Mar 2024
# Any comments or error reporting, please contact Jackson Powell. jap16f@fsu.edu

# R Version 4.3.1 (2023-06-16 ucrt) -- "Beagle Scouts"

# Set up data
mouths <- c(100, 60) # number of mouths, approximate based on Fig 3a x Fig 3b
rate <- c(0.5, 1.5) # per zooid, approximate based on Fig 2b
ratef <- c(0.5, 4) # per feeding zooid, approximate based on Fig 2c



# Prepare Figure 4 
# windows(width = 5,height = 5) # use "quartz()" on Mac
quartz(width = 5,height = 5)

par(mfrow = c(3, 3), mar = c(2, 4, 1, 1), oma = c(1, 0, 1, 0))

# Set up figure parameters
xlims <- c(17, 33)
rate.ylims <- c(0, 5)
mouths.ylims <- c(0, 100)
cex <- 0.9
cex.axis <- 0.8
lwd <- 3


# Row 1
plot(1, type="n", las = 1,bty = "l",  xaxt = "n", yaxt = "n", xlab = "", ylab = "",
     xlim = xlims, ylim = mouths.ylims, cex.axis = cex.axis)
lines(xlims, rep(mean(mouths), 2), col = "tomato", lwd = lwd)

plot(1, type = "n", las = 1,bty = "l",  xaxt = "n", yaxt = "n", xlab="", ylab = "",
     xlim = xlims, ylim = rate.ylims, cex.axis = cex.axis)
lines(xlims, c(rate[1], rate[2]), col = "tomato", lwd = lwd)
mtext(side=2,"x", line = 2.5, cex = 1.5,las=1)

plot(1, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab  = "")

# Row 2
plot(1, type = "n", las = 1, bty = "l",  xaxt = "n", yaxt = "n", xlab="", ylab = "",
     xlim = xlims, ylim = mouths.ylims, cex.axis = cex.axis)
lines(xlims, c(mouths[1],mouths[2]), col = "dodgerblue", lwd = lwd)
mtext("Number of feeding zooids (observable)", side = 2, line = 0.5, cex = cex)


plot(1, type = "n", las = 1, bty = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
     xlim = xlims, ylim = rate.ylims, cex.axis = cex.axis)
lines(xlims, rep(mean(rate), 2), col = "dodgerblue", lwd = lwd)
mtext("Clearance rate per feeding zooid (inferred only)", side = 2, line = 0.5, cex = cex)
mtext(side = 2,"x", line = 2.5, cex = 1.5,las=1)

plot(1, type = "n", las = 1, bty = "l", xaxt = "n", yaxt = "n", xlab = "", ylab  = "",
     xlim = xlims, ylim = c(0,200), cex.axis  = cex.axis)

lines(xlims, mean(mouths) * c(rate[1],rate[2]), col = "tomato", lwd = lwd)
lines(xlims, c(mouths[1], mouths[2]) * mean(rate), col = "dodgerblue", lwd = lwd)
lines(xlims, c(mouths[1], mouths[2]) * c(rate[1], rate[2]), col = "black", lwd = lwd)
lines(xlims, c(mouths[1], mouths[2]) * c(rate[1], rate[2]+1), col = "black", lwd = lwd, lty=3)

mtext("Clearance rate per colony (observable)", side = 2, line = 0.5, cex = cex)
mtext(side = 2, "=", line = 2.5, cex = 2, las = 1)
# Add x-axis label
mtext("Temperature", side = 1, line = 1, cex = cex)

# Row 3 
plot(1, type = "n", las = 1, bty = "l", xaxt = "n", yaxt = "n", xlab="", ylab = "",
     xlim = xlims, ylim = mouths.ylims, cex.axis = cex.axis)
lines(xlims, c(mouths[1], mouths[2]), lwd = lwd)
# Add x-axis label
mtext("Temperature", side = 1, line = 1, cex = cex)

plot(1, type="n", las = 1,bty = "l",  xaxt = "n", yaxt = "n", xlab="", ylab = "",
     xlim = xlims, ylim = rate.ylims, cex.axis = cex.axis)
lines(xlims, c(rate[1], rate[2]), lwd = lwd)
lines(xlims, c(ratef[1], ratef[2]), lwd = lwd, lty = 3)

mtext(side = 2,"x", line = 2.5, cex = 1.5,las=1)
# Add x-axis label
mtext("Temperature", side = 1, line = 1, cex = cex)

plot(1, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab  = "")


