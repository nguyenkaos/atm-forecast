
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
        data.x <- model.matrix (formula, data)

        # remove features that have little/no variance
        #
        # TODO - ignore these columns significantly reduces the score.  but ultimately
        # in my tests it is ignore mostly the "global trends." if I comment out this
        # code, but turn off the generate of the "global trends" the score is much higher.
        # this must indicate that my implementation of this code is not correct and is
        # negatively impacting the score.
        #
        #ignore <- nearZeroVar (data.x, 99/1)
        #loginfo("Ignoring feature with little/no variance: %s", colnames(data.x)[ignore])
        #data.x <- subset (data.x, select = -ignore, drop = F)   
        
        # split the training and test data
        train.index <- which ( data.x[ ,"trandate"] < split.at )
        
        # TODO CANNOT ASSUME THE TARGET IS USAGE
        train.x <- data.x [train.index, ]
        train.y <- data [train.index, usage]
        loginfo("%s: training with '%s' obs and '%s' features prior to '%s'.", 
                by, nrow(train.x), ncol(train.x), split.at)
        
        # no shirt, no shoes, no data = no training
        if (nrow (train.x) <= 0) {
            return (NULL)
        }
        
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
        data.x <- model.matrix (formula, data)
        loginfo("%s: predicting for '%s' all test/train obs.", by, nrow (data.x))
        prediction <- round (predict (fit, newdata = data.x))
    }
    
    return (prediction)
}
