library("caret")

#
# Scores the model based on the given input data.
#
score <- function(data, fit) {
  
  # predict the training set 
  data$usageHat <- predict(fit, newdata=data)
  data$mape <- mapply(mape, data$usage, data$usageHat)
  data$score <- mapply(points, data$mape)
  
  return(data)  
}

#
# Calculates the "mean adjusted percent error" aka MAPE.
#
mape <- function(actual, predict) {
  mape <- NA
  
  if(!is.na(actual))
    mape <- abs((actual - predict) / actual)
  
  return(mape)
}

#
# The number of points is calculated based on the mape
#
points <- function(mape) {
  score <- NA
  
  if(is.na(mape))
    score <- NA
  else if(mape <= 0.05) 
    score <- 2.0
  else if(mape <= 0.10)
    score <- 1.0
  else if(mape <= 0.20)
    score <- 0.5
  else
    score <- 0.0
  
  return(score)
}

