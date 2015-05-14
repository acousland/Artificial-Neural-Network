Threshold_Optimise <- function(data,min=0,max=1,step=0.1)
{
  #initialise results dataframe
  result <- data.frame("Threshold"=0,"Negative"=0,"Positive"=0)
  
  for (i in seq(min,max,by=step))
  {
    threshold <- i
    data.threshold <- data
    data.threshold$PrFault2 <- ifelse(data.threshold$PrFault<threshold,0,data.threshold$PrFault)
    
    # Measure performance
    result.tmp <- data.threshold %>%
      group_by(FAULT) %>%
      summarise (Score = sum(PrFault2))
    
    result <- rbind(result,c(threshold,result.tmp$Score))
  }
  result$success <- result$Positive/result$Negative
  return(result)
}
# Threshold testing
