
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
trainAndPredict <- function (formula,
                             data,
                             by, 
                             split.at, 
                             cache.prefix,
                             default.predict,
                             train.control,
                             method,
                             x.ignore = NA,
                             ...) {
    by <- by[[1]]
    
    fit.cache <- sprintf ("%s-%s-%s", cache.prefix, method, by)
    fit <- cache (fit.cache, {
        
        # build the design matrix based on the formula (will rm any usage = NAs)
        all <- data
        all.x <- model.matrix (formula, all)
        
        # split the training and test data
        train.index <- which ( all.x[ ,"trandate"] < split.at )
        train.x <- all.x [train.index, ]
        train.y <- all [train.index, usage]    # TODO CANNOT ASSUME THE TARGET IS USAGE
        
        loginfo("%s: training with '%s' obs and '%s' features prior to '%s'.", 
                by, nrow(train.x), ncol(train.x), split.at)
        
        # no shirt, no shoes, no data = no training
        if (nrow (train.x) <= 0) {
            return (NULL)
        }
        
        # remove features that have little/no variance
        ignore <- nearZeroVar (train.x)
        loginfo("Ignoring feature with little/no variance: %s", colnames(train.x)[ignore])
        train.x <- subset (train.x, select = -ignore, drop = F)    
        
        loginfo("-----> training with %s", colnames (train.x))
        
        # train the model
        fit <- train (x         = train.x, 
                      y         = train.y, 
                      method    = method, 
                      trControl = train.control, 
                      ...)
    })
    
    # make prediction based on the model - predict for all test/train
    prediction <- default.predict 
    if (!is.null (fit)) {
        loginfo("%s: predicting for '%s' all test/train obs.", by, nrow (all.x))
        prediction <- round (predict (fit, newdata = all.x))
    }
    
    return (prediction)
}

trainAndPredict.experimental <- function (formula,
                                          data,
                                          by, 
                                          split.at, 
                                          cache.prefix,
                                          default.predict,
                                          train.control,
                                          method,
                                          x.ignore = NA,
                                          ...) {
    by <- by[[1]]
    
    fit.cache <- sprintf ("%s-%s-%s", cache.prefix, method, by)
    fit <- cache (fit.cache, {
        
        train <- data [ trandate <= split.at ]
        loginfo("%s: training with '%s' obs and '%s' features prior to '%s'.", 
                by, nrow(train), ncol(train), split.at)
        
        # no shirt, no shoes, no data = no training
        if (nrow (train) <= 0) {
            return (NULL)
        }
        
        # remove features that have little/no variance
        #ignore <- nearZeroVar (train)
        #loginfo("Ignoring feature with little/no variance: %s", colnames(train)[ignore])
        #train <- subset (train, select = -ignore, drop = F)    
        
        # train the model
        fit <- train (form      = formula,
                      data      = train,
                      method    = method, 
                      trControl = train.control, 
                      ...)
    })
    
    # make prediction based on the model - predict for all test/train
    prediction <- default.predict 
    if (!is.null (fit)) {
        loginfo("%s: predicting for '%s' all test/train obs.", by, nrow (data))
        prediction <- round (predict (fit, newdata = data))
    }
    
    return (prediction)
}
