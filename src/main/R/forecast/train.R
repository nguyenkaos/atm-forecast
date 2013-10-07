
onError <- function(e) {
    print(e) 
    return(NULL)
}

##################################################################
# Trains and scores a subset of data.  Returns the entire input data
# set with all features, along with the prediction and scoring
# metrics.  
##################################################################
trainAndScore <- function(by, data, method, splitAt, default, ...) {
    atm <- by$atm
    loginfo("Training '%s' with train/test split at '%s'", atm, splitAt)
    
    # build and cache the fitted model
    cacheAs <- sprintf("fit-%s", atm)
    fit <- cache(cacheAs, {
        trainer(data, method, splitAt, ...)
    })

    # score the model
    result <- NULL
    if(!is.null(fit)) {
        scored <- score(data, fit)
        result <- list(scored$usageHat, scored$mape, scored$score)  
    } 
    return(result)
}

##################################################################
# Trains a subset of data.
##################################################################
trainer <- function(data, method, splitAt, default, f=usage~., ...) {
    fit <- NULL
    
    train <- subset(data, trandate < splitAt)
    if(nrow(train) > 0) {
        ctrl <- trainControl(method="boot632", 
                             number=5, 
                             repeats=3, 
                             allowParallel=T)
        tryCatch(
            fit <- train(f, data=train, method=method, trControl=ctrl, ...), 
            error=onError
        )
        
        # if parameter tuning failed, use the defaults
        if(is.null(fit)) {
            warning("could not find tuning parameters!", immediate.=T)
            tryCatch( 
                fit <- train(f, data=train, method=method, tuneGrid=default), 
                error=onError
            )      
        }  
    }
    
    return(fit)
}
