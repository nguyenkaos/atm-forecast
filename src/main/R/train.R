# RESULTS:
# RF on sample -> rmse = 2172.41  r2 = 0.97  score = 46.87%
#
# Trains a model based on input, a function, and a method.
#
trainer <- function(inputData, 
    f = usage ~ sin(dayOfYear*2*pi) + sin(dayOfSemiYear*2*pi) + sin(dayOfQuarter*2*pi) + dayOfWeek + weekOfMonth + weekOfYear + paydayN + holidayN + eventDistance,
    method = "gbm", 
    defaultTuneGrid = expand.grid(.interaction.depth=2, .n.trees=50, .shrinkage=0.1),
    p = ymd("2013-05-15")) {
  
  # split the data for cross-validation
  train <- subset(inputData, trandate < as.Date(p))
  test <- subset(inputData, trandate >= as.Date(p))

  # some of the sub sets may not have any data
  if(nrow(train) == 0 || nrow(test) == 0) {
    warning("the size of either the train or test set is 0")
    return(data.frame()) 
  }
  
  # mark the training vs test sets
  test$isTest <- T 
  train$isTest <- F
  
  # use k-fold cross-validation to tune the parameters
  trControl <- trainControl(method="repeatedcv", number=5, repeats=5)
  
  tryCatch(
    fit <- train(f, data=train, method=method, trControl=trControl), # GBM -> verbose=F, distribution="poisson"),
    error = function(e) {
      print(e) 
      return(NULL)
    }
  )
  
  if(!exists("fit")) {
    warning("could not find tuning parameters!")
    
    # since parameter prediction failed, use the defaults
    tryCatch(
      fit <- train(f, data=train, method=method, tuneGrid=defaultTuneGrid), #GBM -> verbose=F, distribution="poisson"),
      error = function(e) {
        print(e) 
        return(NULL)
      }
    )      
  }
  
  return(fit)
}


