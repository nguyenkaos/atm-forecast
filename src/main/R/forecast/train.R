
library("logging")

#
# Logs any errors encountered
#
onError <- function (e) {
    logerror(e)
    return(NULL)
}

#
# Trains and scores a subset of data.  Returns the entire input data
# set with all features, along with the prediction and scoring
# metrics.  
#
trainAndPredict <- function (by, data, method, split.at, default, cache.prefix, formula = usage ~ ., ...) {
    by <- by[[1]]
    loginfo("Training group '%s' split at '%s' with '%s' obs.", by, split.at, nrow(data))
    
    cacheAs <- sprintf ("%s-%s", cache.prefix, by)
    fit <- cache (cacheAs, {
        
        # train the predictive model
        trainer (data, method, split.at, formula, default, ...)
    })
    
    prediction <- -2
    if (!is.null (fit)) {
        
        # make a prediction based on the fitted model
        prediction <- predict (fit, newdata = data)
    }
    
    return (prediction)
}

#
# Trains a subset of data.
#
trainer <- function (data, method, split.at, formula, default,  ...) {
    fit <- NULL
    
    train <- subset (data, trandate < split.at)
    if (nrow (train) > 0) {
        ctrl <- trainControl(method        = "boot632", 
                             number        = 5,
                             repeats       = 3,
                             returnData    = FALSE,
                             allowParallel = TRUE )
        tryCatch(
            fit <- train (formula, data = train, method = method, trControl = ctrl, ...), 
            error = onError
        )
        
        # if parameter tuning failed, use the defaults
        if (is.null (fit)) {
            warning ("could not find tuning parameters!", immediate. = T)
            tryCatch ( 
                fit <- train (formula, data=train, method=method, tuneGrid=default, ...), 
                error = onError
            )      
        }  
    }
    
    return (fit)
}
