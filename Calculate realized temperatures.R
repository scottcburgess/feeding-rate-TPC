### This code produces realized temperature values of water baths in the manuscript: 
### Powell, JA and  SC Burgess. How modularity and heterotrophy complicate the understanding of the causes of thermal performance curves: the case of feeding rate in a filter feeding animal
# Code finalized Mar 2024
# Any comments or error reporting, please contact Jackson Powell. jacksonpowell129@gmail.com

# R Version 4.3.1 (2023-06-16 ucrt) -- "Beagle Scouts"

# Import data
# Set.wd() # set working directory, or file path, before importing data

dat <- read.csv("Temperature logger data.csv")
dat$batch_logger <- interaction(dat$batch, dat$logger)

# Create data frame for realized temperature values to fill 
target.temps <- c(18, 20, 22, 24, 26, 28, 30, 32)
batch = c(1, 2)
temp.summary <- expand.grid(Target.Temperature = target.temps, Batch=batch)
temp.summary <- temp.summary[order(temp.summary$Target.Temperature,temp.summary$Batch),]  # should be in the same order as dat.list

# Visualize temperature over time for each logger
ind <- unique(dat$batch_logger)
windows() # use "quartz()" on Mac
par(mfrow = c(4 ,4), mar = c(1, 1, 1, 1), oma = c(0, 0, 0, 0))
for(i in 1:length(ind)){
  tmp <- dat[dat$batch_logger == ind[i], ]
  from <- 25 # Time log when the experiment began (~6 hours after heaters were activated and Bugula were placed into water baths)
  to <- 280 # Near time log when the experiment ended
  with(tmp, plot(log, temp, type = "l", ylab = "", xlab = "", xaxt = "n",yaxt = "n", ylim = c(17, 35)))
  with(tmp[from:to, ],lines(log, temp, col = "red"))
  mtext(side = 3,eval(paste(ind[i], unique(tmp$target.temp))), lwd = 2)
}

windows() # use "quartz()" on Mac
plot(1, type = "n", xlab = "Target Temperature", ylab = "Realized Temperature", xlim = c(18, 33), ylim = c(15, 33))
ind <- unique(dat$batch_logger)
for(i in 1:length(ind)){
  tmp <- dat[dat$batch_logger == ind[i], ]
  from <- 25 # Time log when the experiment began (~6 hours after heaters were activated and Bugula were placed into water baths)
  to <- 280 # Near time log when the experiment ended
  with(tmp[from:to, ], lines(target.temp, temp,col=batch_logger))
}



# Calculate means, variance metrics, and ranges for logged temperatures 
temp.summary$Mean.Temp <- 0
temp.summary$SD.Temp <- 0
temp.summary$Median.Temp <- 0
temp.summary$Min.Temp <- 0
temp.summary$Max.Temp <- 0
temp.summary$Diff.Temp <- 0

from.min <- 25
to.min <- 280

for(i in 1:length(ind)){ 
  tmp <- dat[dat$batch_logger == ind[i], ]
  temp.summary[i, which(names(temp.summary) == "Mean.Temp")] <- round(with(tmp[from.min:to.min, ], mean(temp)), 2)
  temp.summary[i, which(names(temp.summary) == "SD.Temp")] <- round(with(tmp[from.min:to.min,] , sd(temp)), 2)
  temp.summary[i, which(names(temp.summary) == "Median.Temp")] <- round(with(tmp[from.min:to.min, ], median(temp)), 2)
  temp.summary[i, which(names(temp.summary) == "Min.Temp")] <- round(with(tmp[from.min:to.min, ], min(temp)), 2)
  temp.summary[i, which(names(temp.summary) == "Max.Temp")] <- round(with(tmp[from.min:to.min, ], max(temp)), 2)
  temp.summary[i, which(names(temp.summary) == "Diff.Temp")] <- round(with(tmp[from.min:to.min, ], max(temp) - min(temp)), 2)
}

# Add median temperatures for each tank and batch to "temp.summary" data frame
temp.summary$tankID <- with(temp.summary, paste(Target.Temperature, ifelse(Batch == 1, ".b1", ".b2"),sep = ""))
temp.summary$tankID <- paste0("t", temp.summary$tankID)

# write.csv(temp.summary,"Temperature_summary.csv",row.names=F)
