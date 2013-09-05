
onError <- function(e) {
    print(e) 
    return(NULL)
}

##################################################################
# Trains a model based on the given data, formula, and 
# tuning parameters.  The 'fit' produced by the model is returned.
##################################################################
trainer <- function(data, formula, method, defaultTuneGrid, p, ...) {
    fit <- NULL
    
    # split the data for cross-validation; p specifies the break point
    train <- subset(data, trandate < as.Date(p))
    
    # ensure that there is enough data to train/tune
    if(nrow(train) == 0) {
        warning(sprintf("unable to train: nrow(train)=%.0f", nrow(train)))
        return(NULL) 
    }
    
    # tune/train the model - use k-fold cross-validation to tune the model parameters
    control <- trainControl(method="repeatedcv", number=5, repeats=5, returnData=T)
    tryCatch(
        fit <- train(form=formula, data=train, method=method, trControl=control, ...), 
        error=onError
    )
    
    # if parameter tuning failed, use the defaults
    if(is.null(fit)) {
        warning("could not find tuning parameters!", immediate.=T)
        tryCatch( 
            fit <- train(form=formula, data=train, method=method, tuneGrid=defaultTuneGrid),
            error=onError
        )      
    }
    
    return(fit)
}


