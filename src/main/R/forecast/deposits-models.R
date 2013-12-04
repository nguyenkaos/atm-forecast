library("caret")
library("data.table")
library("logging")
library("foreach")
library("Metrics")
library("caretEnsemble")
library("plyr")

#
# fetch the current champion model
#
champion <- function (features, champion.file = "../../resources/deposits-champion.rds") {

    models <- readRDS (champion.file)
    champion <- models [ features, 
                         list (usage, usage.hat = usage.champ, model = "champion")] 
}

#
# fetch the naive model
#
naive <- function (features, naive.file = "../../resources/deposits-champion.rds") {
    
    # grab the naive model
    models <- readRDS (naive.file)
    naive <- models [ features, 
                      list (usage, usage.hat = usage.naive, model = "naive")]
}

#
# train the model
#
deposits.train <- function (by, data.x, data.y, train.index, data.id) {
    fit <- NULL
    
    # cache the trained model
    fit.cache <- sprintf ("%s-challenger-%s", data.id, by)
    fit <- cache (fit.cache, {
        loginfo("[%s] pre-processing: [%s x %s]", by, nrow(data.x), ncol(data.x))
        features.before <- colnames (data.x)
        
        # remove features that are highly correlated or with little/no variance
        data.x <- data.x[, -nearZeroVar (data.x)]
        data.x <- data.x[, -findCorrelation (data.x)]
        logdebug ("[%s] low variance/correlation detected: %s", by, sort (setdiff (features.before, colnames (data.x))))
        
        # split the training and test data
        train.x <- data.x [  train.index, ]
        train.y <- data.y [  train.index  ]
        test.x  <- data.x [ -train.index, ]
        test.y  <- data.y [ -train.index  ]
        
        # if no training data, or training response all 0s then don't train
        loginfo("[%s] training: [%s x %s]", by, nrow(train.x), ncol(train.x))
        if (nrow (train.x) > 0 && any (train.y > 0)) {
            
            # TODO THIS IS NOT IMPUTING USAGE THE RESPONSE VARIABLE
            
            # default args for each of the models
            args.default = list (
                x          = train.x, 
                y          = train.y, 
                
                # defines pre-processing 
                preProcess = c("center", "scale", "knnImpute"),
                
                # defines how tuning/training should occur
                trControl  = trainControl (
                    method           = "cv",
                    number           = 5,
                    classProbs       = T,
                    returnData       = F,
                    savePredictions  = T,
                    allowParallel    = T,
                    verbose          = F,
                    returnResamp     = "none", 
                    predictionBounds = c(0, mean(train.x) + 4 * sd(train.y)),
                    index            = createFolds (train.y, k = 5)
                )
            )
            
            # define each of the challenger models
            args.custom <- list ( 
                list (method = "gbm", verbose = F),
                list (method = "glmboost"),
                list (method = "lasso"),
                list (method = "leapForward", warn.dep = F),
                list (method = "knn")
            )
            
            # train each of the challengers; ignore any training failures
            challengers <- lapply.ignore (args.custom, function (args) do.call (caret::train, append (args.default, args)))
            loginfo ("[%s] trained '%s' model(s) for ensembling", by, length (challengers))
            
            # create a greedy ensemble 
            if (length (challengers) > 0) {
                fit <- caretEnsemble (challengers, iter = 1000L)
            } 
        }
    })
    
    loginfo ("[%s] ensemble chosen with rmse: %.2f models: %s", by, fit$error, format.wide (sort (fit$weights, decreasing = T)))
    return (fit)
}

#
# makes predictions based on a fitted model
#
deposits.predict <- function (by, fit, data.x, default.predict = 0) {
    
    # the default prediction, just in case shit hits the fan
    prediction <- rep (default.predict, nrow (data.x))
    if (!is.null (fit)) {
        
        # extract only the features used to train the model
        feature.names <- fit$models[[1]]$finalModel$xNames
        data.x <- data.x [, feature.names]
        
        # make a prediction - predict for all test/train
        prediction <- predict (fit, newdata = data.x)
        prediction <- getOrElse( round (prediction), default.predict)
    }
    
    loginfo("[%s] prediction: [%s x %s]: %s", by, nrow (data.x), ncol (data.x), format.wide (summary (prediction)))
    return (prediction)
}

#
# train the challenger and make a prediction for a specific ATM
#
trainThenPredict <- function (by,
                              data, 
                              data.id,
                              formula = usage ~ . -1 -train ) {
    by <- by[[1]]
    train.index <- which (data[["train"]] == 1)
    
    # create the design matrix
    frame <- model.frame (formula, data, na.action = na.pass)
    data.y <- model.response (frame)
    data.x <- model.matrix (formula, frame)
    
    # train and predict
    fit <- deposits.train (by, data.x, data.y, train.index, data.id)
    pred <- deposits.predict (by, fit, data.x)
    
    return (pred)
}

#
# train a challenger model.  this model uses GBM and performs centering
# and scaling of the 
#
challenger <- function (features, 
                        subset    = opts$subset, 
                        data.id   = basename.only (opts$historyFile)) {
    
    features [
        # include only those ATMs that pass the 'subset' expression
        eval (parse (text = subset)),
        
        # train and fit a model
        list (
            trandate,
            usage,
            usage.hat = trainThenPredict (.BY, .SD, data.id),
            model     = "challenger"
        ),
        
        # training occurs independently for each ATM
        by = atm ] 
}
