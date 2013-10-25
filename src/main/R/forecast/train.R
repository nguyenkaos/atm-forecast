
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
trainAndPredict <- function (by, 
                             data, 
                             method, 
                             split.at, 
                             cache.prefix, 
                             default.tune,
                             train.control,
                             formula = usage ~ ., 
                             default.predict = 0.0, 
                             ...) {
    by <- by[[1]]
    loginfo("Training group '%s' split at '%s' with '%s' obs.", by, split.at, nrow(data))
    
    # cache the trained model
    fit <- cache (sprintf ("%s-%s-%s", cache.prefix, method, by), {
        fit <- NULL
        
        # split the training and test data
        train <- subset (data, trandate < split.at)
        if (nrow (train) > 0) {
            
            # train the predictive model
            tryCatch(
                fit <- train (formula, 
                              data      = train, 
                              method    = method, 
                              trControl = train.control, 
                              ...), 
                error = onError
            )
            
            # if parameter tuning failed, use the defaults
            if (is.null (fit)) {
                warning ("could not find tuning parameters!", immediate. = T)
                tryCatch ( 
                    fit <- train (formula, data = train, method = method, tuneGrid = default.tune, ...), 
                    error = onError
                )      
            }  
        }
        
        fit
    })
    
    prediction <- default.predict 
    if (!is.null (fit)) {
        
        # make a prediction based on the fitted model
        prediction <- predict (fit, newdata = data)
    }
    
    return (prediction)
}
