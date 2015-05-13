require (neuralnet)    # Nerual Network Package
require (RODBC)        # Load RODBC package
require (lubridate)   # Required to manipulate dates

# Create a connection to the database called "channel"
local.connection <- odbcConnect("RTV", believeNRows=FALSE)

# Query the database and put the results into the data frame "LoggingResults"
logger.results <- sqlQuery(local.connection, 
                           "SELECT DATEANDTIME, SECONDS, RMSI1, RMSI3 from ELSPEC_LOGGER_HARMONICS;")

test.times <- sqlQuery(local.connection, "SELECT * from TEST_TIMES_CONFIRMED;")

# Initialise timestamps to consistent format
logger.results$TIMESTAMP <- dmy_hm(logger.results$DATEANDTIME) + as.numeric(logger.results$SECONDS)

logger.results$testing <- 0

for (i in 1:nrow(test.times))
{
  for (j in 1:nrow(logger.results))
  {
    if(logger.results$TIMESTAMP[j]>test.times$STARTTIME[i] && logger.results$TIMESTAMP[j]<test.times$ENDTTIME[i])
    {
      logger.results$testing[j] <- 1
    }
  }
}

logger.results[1,]

ifelse(logger.results$TIMESTAMP == 3 , 1 ,)

EndTime <- as.POSIXct("2015-03-17 12:20:00", format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
StartTime <- as.POSIXct("2015-03-17 12:15:00", format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")

filtered.results <- subset(logger.results, logger.results$TIMESTAMP >= StartTime & logger.results$TIMESTAMP <= EndTime)

plot(filtered.results$TIMESTAMP, filtered.results$RMSI1)



#Generate 50 random numbers uniformly distributed between 0 and 100
#And store them as a dataframe
traininginput <-  as.data.frame(runif(50, min=0, max=100))
trainingoutput <- sqrt(traininginput)

#Column bind the data into one variable
trainingdata <- cbind(traininginput,trainingoutput)
colnames(trainingdata) <- c("Input","Output")

#Train the neural network
#Going to have 10 hidden layers
#Threshold is a numeric value specifying the threshold for the partial
#derivatives of the error function as stopping criteria.
net.sqrt <- neuralnet(Output~Input,trainingdata, hidden=50, threshold=0.001)
print(net.sqrt)

#Plot the neural network
plot(net.sqrt)

#Test the neural network on some training data
testdata <- as.data.frame((1:10)^2) #Generate some squared numbers
net.results <- compute(net.sqrt, testdata) #Run them through the neural network

#Lets see what properties net.sqrt has
ls(net.results)

#Lets see the results
print(net.results$net.result)

#Lets display a better version of the results
cleanoutput <- cbind(testdata,sqrt(testdata),
                     as.data.frame(net.results$net.result))
colnames(cleanoutput) <- c("Input","Expected Output","Neural Net Output")
print(cleanoutput)