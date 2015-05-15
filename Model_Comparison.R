#########################################################
# Artificial Neural Network - Model Comparison 
# 
# - Aaron Cousland 15/05/2015
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

# Create backup of datasets
logger.results.training.backup <- logger.results.training
logger.results.validation.backup <- logger.results.validation

# Set NN model parameters
model.size <- 10
decay.threshold <- 0.001
max.iterations <- 1000

for (i in 1:100)
{
  logger.results.training <- logger.results.training.backup
  logger.results.validation <- logger.results.validation.backup

  # Superimpose load current on fault current
  # logger.results.training$RMSI1 <- (logger.results.training$RMSI1 + logger.results.training$RMSI2)
  # logger.results.validation$RMSI1 <- (logger.results.validation$RMSI1 + logger.results.validation$RMSI2)
  
  # Train neural network
  NeuralModel = nnet(FAULT~RMSI1, data=logger.results.training,size=model.size,maxit=max.iterations,decay=decay.threshold)
  
  # Make predictions based on neural network
  logger.results.validation$PrFault <- predict(NeuralModel,logger.results.validation) 
  
  # Optimise the trigger threshold
  results <- Threshold_Optimise(logger.results.validation,0,1,0.05)
  threshold <- results[which.max(results[,4]),1]
  
  # Perform thresholding as per otimum value
  logger.results.validation$FtDetected <- ifelse(logger.results.validation$PrFault<threshold,0,1)
  #logger.results.validation$FtDetected <- logger.results.validation$PrFault
  
  # Measure performance
  performance <- logger.results.validation %>%
    group_by(FAULT) %>%
    summarise (Score = sum(FtDetected))
  print(paste("Score =",performance$Score[2]-performance$Score[1],"/",sum(logger.results$FAULT==TRUE)))
  
  # Write to results table
  write.table(t(c(model.size,decay.threshold,max.iterations,performance$Score,performance$Score[2]-performance$Score[1])),file="Model_Comparison_Results.csv", sep=",",append=T, row.names=F, col.names = F)
}

