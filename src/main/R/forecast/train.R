
onError <- function(e) {
    print(e) 
    return(NULL)
}

##################################################################
# Trains and scores a subset of data.  Returns the entire input data
# set with all features, along with the prediction and scoring
# metrics.  
##################################################################
trainAndScore <- function(by, data, method="gbm", splitAt="2013-07-01", ...) {
    atm <- by$atm
    loginfo("Training '%s' with test/train split at '%s'", atm, splitAt)
    
    # build and cache the fitted model
    fit <- cache(sprintf("fit-%s", atm), { 
        
        # train the model   
        fit <- trainer(data, 
                       form=usage ~ 
                           # date related features
                           trandate.n + day.of.month + day.of.year + day.of.semi.year + day.of.quarter + 
                           day.of.week + week.of.month + week.of.year + quarter + month.of.year + 
                           
                           # paydays, holidays, TODO(Nick): ignoring events until data updated
                           payday.n + holiday.n + #eventDistance +
                           
                           # usage trends specific to the ATM
                           woy.mean + woy.min + woy.max + woy.sd + 
                           moy.mean + moy.min + moy.max + moy.sd + 
                           dow.mean + dow.min + dow.max + dow.sd + 
                           wom.mean + wom.min + wom.max + wom.sd + 
                           qua.mean + qua.min + qua.max + qua.sd +
                           hol.mean + hol.min + hol.max + hol.sd +
                           pay.mean + pay.min + pay.max + pay.sd +
                           
                           # usage trends across all ATMs
                           woy.all.mean + woy.all.min + woy.all.max + woy.all.sd + 
                           moy.all.mean + moy.all.min + moy.all.max + moy.all.sd + 
                           dow.all.mean + dow.all.min + dow.all.max + dow.all.sd +  
                           wom.all.mean + wom.all.min + wom.all.max + wom.all.sd + 
                           qua.all.mean + qua.all.min + qua.all.max + qua.all.sd + 
                           hol.all.mean + hol.all.min + hol.all.max + hol.all.sd +  
                           pay.all.mean + pay.all.min + pay.all.max + pay.all.sd,
                       p=as.Date(splitAt),
                       method=method,
                       ...)
    })
    
    result <- NULL
    if(!is.null(fit)) {
        # score the model
        scored <- score(data, fit)
        result <- list(scored$usageHat, scored$mape, scored$score)
        
    } else {
        # most likely there was not enough training data for this ATM
        warning("Not enough training data for ATM ", atm)
    }
    
    return(result)
}

##################################################################
# Trains a model based on the given data, formula, and 
# tuning parameters.  The 'fit' produced by the model is returned.
##################################################################
trainer <- function(data, formula, method, default, p, ...) {
    fit <- NULL
    
    # split the data for cross-validation; p specifies the break point
    train <- subset(data, trandate < as.Date(p))
    
    # ensure that there is enough data to train/tune
    if(nrow(train) == 0) {
        warning(sprintf("unable to train: nrow(train)=%.0f", nrow(train)))
        return(NULL) 
    }
    
    # tune/train the model - use k-fold cross-validation to tune the model parameters
    control <- trainControl(method="repeatedcv", number=5, repeats=5, returnData=T, allowParallel=T)
    tryCatch(
        fit <- train(form=formula, data=train, method=method, trControl=control, ...), 
        error=onError
    )
    
    # if parameter tuning failed, use the defaults
    if(is.null(fit)) {
        warning("could not find tuning parameters!", immediate.=T)
        tryCatch( 
            fit <- train(form=formula, data=train, method=method, tuneGrid=default),
            error=onError
        )      
    }
    
    return(fit)
}


