
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
        
        # TODO use a data.table function to split here!!
        
        # split the training and test data
        train <- subset (data, trandate < split.at)
        if (nrow (train) > 0) {
            
            # exclude columns with little/no variance from training
            #preds.exclude <- names(train)[nearZeroVar (train, freqCut = 99/1)]
            #loginfo("excluded with little variance: %s", preds.exclude)
            
            # TODO hack - remove those with little/no variance from formula - ugly
            #preds.orig <- attr( terms(formula), "term.labels")
            #preds.new <- setdiff(preds.orig, preds.exclude)
            #attr( terms(formula), "term.labels")
            #formula <- reformulate(termlabels = preds.new, response = 'usage') 
            
            fit <- train (formula, 
                          data      = train, 
                          method    = method, 
                          trControl = train.control, 
                          ...)
        }
    })
    
    prediction <- default.predict 
    if (!is.null (fit)) {
        
        # make a prediction based on the fitted model
        prediction <- round (predict (fit, newdata = data))
    }
    
    return (prediction)
}
