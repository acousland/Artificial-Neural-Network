##### Still contains model size training loop!!!###

Network_Optimise <- function(data.training,data.validation,equation,model.size=10,max.iterations=1000,decay.threshold=0.001,loops=10)
{
  # Start score below what the worst model could score
  best.score=-1000000
  best.size = 0
  data.validation$FtDetected <- 0
  for (j in seq(5,200,by=1))
  {
    model.size = j
    
    for(i in 1:loops)
    {
      # Train neural network
      NeuralModel = nnet(FAULT~RMSI1, data=data.training,size=model.size,maxit=max.iterations,decay=decay.threshold)
      
      # Make predictions based on neural network
      data.validation$PrFault <- predict(NeuralModel,data.validation) 
      
      # Optimise the trigger threshold
      results <- Threshold_Optimise(data.validation,0,1,0.05)
      threshold <- results[which.max(results[,4]),1]
      
      # Perform thresholding as per otimum value
      data.validation$FtDetected <- ifelse(data.validation$PrFault<threshold,0,1)
      
      # Measure performance
      performance <- data.validation %>%
        group_by(FAULT) %>%
        summarise (Score = sum(FtDetected))
      
      print((performance$Score[2]-performance$Score[1]))
      
      if((performance$Score[2]-performance$Score[1])>best.score)
      {
        best.score <- performance$Score[2]-performance$Score[1]
        best.model <- NeuralModel
        best.size <- model.size  
      }
    } 
  }
  print("The optimal number of nodes is")
  print(model.size)
  return(best.model)
}


