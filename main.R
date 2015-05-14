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
require (devtools)
require (clusterGeneration)

source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

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

# Generate the neural network model
NeuralModel = nnet(FAULT~RMSI1, data=logger.results.training,size=20,maxit=1000,decay=.001)
#save(fault.network, file="test.rda")
#load("test.rda")

logger.results.validation$PrFault <- predict(NeuralModel,logger.results.validation) 

# Measure performance
performance <- logger.results.validation %>%
  group_by(FAULT) %>%
  summarise (Score = sum(PrFault))

print(performance)
print(paste("Score =",performance$Score[2]-performance$Score[1],"/",sum(logger.results$FAULT==TRUE)))

# Interrogate results
StartTime <- force_tz(as.POSIXct("2015-03-17 09:11:00", format = "%Y-%m-%d %H:%M:%OS"),"UTC")
EndTime <- force_tz(as.POSIXct("2015-03-17 15:13:00", format = "%Y-%m-%d %H:%M:%OS"),"UTC")
logger.results.validation2 <- subset(logger.results.validation, logger.results.validation$TS >= StartTime & logger.results.validation$TS <= EndTime)
plot(logger.results.validation2$TS,logger.results.validation2$RMSI1, type="l")
polygon(logger.results.validation2$TS,logger.results.validation2$PrFault*max(logger.results.validation2$RMSI1), col =rgb(1,0,0,alpha=0.3),xlab="",ylab="",yaxt="n",border = NA)
polygon(logger.results.validation2$TS,logger.results.validation2$FAULT*max(logger.results.validation2$RMSI1), col =rgb(0,0,1,alpha=0.3),xlab="",ylab="",yaxt="n",border = NA)
axis(4)
