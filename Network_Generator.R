#########################################################
# Artificial Neural Network Training Routine 
#
# Identification of faults through supervised learning 
# of an artificial neural network
# 
# - Aaron Cousland 14/05/2015
########################################################

require (neuralnet)    # Nerual Network Package
require (RODBC)        # Load RODBC package
require (lubridate)    # Required to manipulate dates

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
fault.network <- neuralnet(FAULT~RMSI1,logger.results.training, hidden=50, threshold=0.001)

save(fault.network, file="Model1.rda")

