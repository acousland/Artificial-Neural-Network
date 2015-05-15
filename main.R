#########################################################
# Artificial Neural Network Implementation 
#
# Identification of faults through supervised learning 
# of an artificial neural network
# 
# - Aaron Cousland 14/05/2015
########################################################

require (nnet)         # Nerual Network Package
require (RODBC)        # Load RODBC package
require (lubridate)    # Required to manipulate dates
require (dplyr)        # Required for performance measurement

source ("Threshold_Optimise.R")

# Create a connection to the database called "RTV"
odbcCloseAll()
local.connection <- odbcConnect("RTV", believeNRows=FALSE)

# Query the database and put the results into the data frame logging.results
logger.results <- sqlQuery(local.connection,"SELECT * FROM ELSPEC.RMS_TRAINING where ts between '17/Mar/15 08:00:00 AM' and '17/Mar/15 03:00:00 PM';")
odbcCloseAll()

# Order by timestamp and force local timestamp
logger.results <- logger.results[with(logger.results, order(logger.results$TS)),]
logger.results$TS <- force_tz(logger.results$TS,"UTC")

# Create Training Dataset
StartTime <- as.POSIXct("2015-03-17 09:00:00", format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
EndTime <- as.POSIXct("2015-03-17 11:00:00", format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
logger.results.training <- subset(logger.results, logger.results$TS >= StartTime & logger.results$TS <= EndTime)

# Create Validation Dataset
StartTime <- as.POSIXct("2015-03-17 08:00:00", format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
EndTime <- as.POSIXct("2015-03-17 15:00:00", format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
logger.results.validation <- subset(logger.results, logger.results$TS >= StartTime & logger.results$TS <= EndTime)

# Create Testing Dataset
StartTime <- as.POSIXct("2015-03-17 08:00:00", format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
EndTime <- as.POSIXct("2015-03-17 15:00:00", format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
logger.results.testing <- subset(logger.results, logger.results$TS >= StartTime & logger.results$TS <= EndTime)

# Superimpose load current on fault current
logger.results.training$RMSI1 <- (logger.results.training$RMSI1 + logger.results.training$RMSI2)
logger.results.validation$RMSI1 <- (logger.results.validation$RMSI1 + logger.results.validation$RMSI2)
logger.results.testing$RMSI1 <- (logger.results.testing$RMSI1 + logger.results.testing$RMSI2)

source("Network_Optimise.R")

model.size=5
max.iterations=1000
decay.threshold=0.001
model.instances=10

Network_Details <- Network_Optimise(logger.results.training
                 ,logger.results.validation
                 ,"FAULT~RMSI1"
                 ,model.size
                 ,max.iterations
                 ,decay.threshold
                 ,model.instances)

logger.results.testing$PrFault <- predict(Network_Details,logger.results.testing)

# Optimise the trigger threshold
results <- Threshold_Optimise(logger.results.testing,0,1,0.05)
threshold <- results[which.max(results[,4]),1]
rm(results)

# Perform thresholding as per otimum value
logger.results.testing$FtDetected <- ifelse(logger.results.testing$PrFault<threshold,0,1)

# Measure performance
performance <- logger.results.testing %>%
  group_by(FAULT) %>%
  summarise (Score = sum(FtDetected))

print(performance$Score[2]-performance$Score[1])

# Interrogate results
StartTime <- force_tz(as.POSIXct("2015-03-17 13:18:00", format = "%Y-%m-%d %H:%M:%OS"),"UTC")
EndTime <- force_tz(as.POSIXct("2015-03-17 13:19:00", format = "%Y-%m-%d %H:%M:%OS"),"UTC")
logger.results.validation.subset <- subset(logger.results.validation, logger.results.validation$TS >= StartTime & logger.results.validation$TS <= EndTime)

plot(logger.results.validation.subset$TS,logger.results.validation.subset$RMSI1, type="l")
#plot(logger.results.validation.subset$TS,logger.results.validation.subset$FtDetected*max(logger.results.validation.subset$RMSI1))
polygon(logger.results.validation.subset$TS,logger.results.validation.subset$FtDetected*max(logger.results.validation.subset$RMSI1), col =rgb(1,0,0,alpha=0.3),xlab="",ylab="",yaxt="n",border = NA)
#polygon(logger.results.validation.subset$TS,logger.results.validation.subset$FAULT*max(logger.results.validation.subset$RMSI1), col =rgb(0,0,1,alpha=0.3),xlab="",ylab="",yaxt="n",border = NA)
axis(4)
