# RESULTS:
# RF on sample -> rmse = 2172.41  r2 = 0.97  score = 46.87%
#
# Trains a model based on input, a function, and a method.
#
trainer <- function(data, form, method, defaultTuneGrid, p) {
  
  # split the data for cross-validation
  train <- subset(data, trandate < as.Date(p))
  test <- subset(data, trandate >= as.Date(p))

  # some of the sub sets may not have any data
  if(nrow(train) == 0 || nrow(test) == 0) {
    warning(sprintf("unable to train: nrow(train)=%.0f nrow(test)=%.0f", nrow(train), nrow(test)))
    return(NULL) 
  }
  
  # mark the training vs test sets
  test$isTest <- T 
  train$isTest <- F
  
  # use k-fold cross-validation to tune the parameters
  trControl <- trainControl(method="repeatedcv", number=5, repeats=5)
  
  tryCatch(
    fit <- train(form=form, data=train, method=method, trControl=trControl), # GBM -> verbose=F, distribution="poisson"),
    error = function(e) {
      print(e) 
      return(NULL)
    }
  )
  
  if(!exists("fit")) {
    warning("could not find tuning parameters!")
    
    # since parameter prediction failed, use the defaults
    tryCatch(
      fit <- train(form=form, data=train, method=method, tuneGrid=defaultTuneGrid), #GBM -> verbose=F, distribution="poisson"),
      error = function(e) {
        print(e) 
        return(NULL)
      }
    )      
  }
  
  return(fit)
}


