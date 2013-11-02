#
# fetch the current champion model
#
champion <- function (features, 
                      split.at, 
                      champion.file = "../../resources/deposits-champion.rds") {
    
    # fetch current champion's forecast 
    champion <- readRDS (champion.file)
    
    # we are only interested in those atm-days in the feature set
    champion <- champion [ 
        features [ trandate > as.Date(split.at) & is.finite(usage) ],
        list (
            usage,
            usage.hat,
            model = "champion"
        )] 
    
    champion <- champion [ is.finite(usage.hat) ]
}

#
# train the challenger and make a prediction for a specific ATM
#
trainThenPredict <- function (by,
                              data, 
                              split.at,
                              data.id,
                              formula = usage ~ . -1,
                              default.predict = 0.0) {
    by <- by[[1]]
    
    # create the design matrix
    frame <- model.frame(formula, data)
    data.y <- model.response(frame)
    data.x <- model.matrix(formula, frame)
    
    # remove features with little to no variance
    data.x <- data.x[, -nearZeroVar(data.x)]
    
    # perform PCA to reduce the feature set
    pre <- preProcess(data.x, method = c("center", "scale")) # pca
    data.x <- predict(pre, data.x)
    
    # split the training and test data
    train.index <- which ( data[["trandate"]] < as.Date (split.at) )
    train.x <- data.x [ train.index, ]
    train.y <- data.y [ train.index  ]
    test.x <- data.x [ -train.index, ]
    test.y <- data.y [ -train.index  ]
    
    # cache the trained model
    fit.cache <- sprintf ("%s-%s", data.id, by)
    fit <- cache (fit.cache, {
        
        loginfo("%s: training with '%s' obs and '%s' features prior to '%s'.", 
                by, nrow(train.x), ncol(train.x), split.at)
        
        # if no training data, or training response all 0s then don't train
        if (nrow (train.x) <= 0 || all(train.y == 0)) {
            return (NULL)
        }
        
        # define how tuning of the models should occur
        ctrl <- trainControl (
            method          = "repeatedcv", #"repeatedcv
            number          = 5, 
            repeats         = 1,
            returnResamp    = "none", 
            classProbs      = TRUE,
            returnData      = FALSE, 
            savePredictions = TRUE,
            allowParallel   = TRUE,
            index = createMultiFolds (train.y, k = 5, times = 1))
        
        # define each of the challenger models
        challengers.def <- list ( 
            list (x = train.x, y = train.y, trControl = ctrl, method = "gbm", distribution = "poisson", verbose = F, keep.data = T),
            list (x = train.x, y = train.y, trControl = ctrl, method = "svmRadial"),                    
            list (x = train.x, y = train.y, trControl = ctrl, method = "glmnet"),
            list (x = train.x, y = train.y, trControl = ctrl, method = "earth"),
            list (x = train.x, y = train.y, trControl = ctrl, method = "lasso"),
            list (x = train.x, y = train.y, trControl = ctrl, method = "nnet", trace = F))
        
        challengers <- lapply (challengers.def, function(args) {
            
            # train each of the challengers
            tryCatch( {
                do.call(train, args)
                
            # ignore any training failures
            }, error = function(e) {
                logwarn("%s: error encountered while training: %s", by, e)
            })  
        })
        
        # remove any NULLs which would indicate failed training for one of the models
        challengers[ sapply(challengers, is.null)] <- NULL
        
        # create a greedy ensemble 
        greedy <- caretEnsemble (challengers, iter = 1000L)
    })
    
    # log information about the trained model
    w <- sort(fit$weights, decreasing = TRUE)
    loginfo("%s: ensemble chosen with rmse: %.2f models: %s", by, fit$error,  
            paste (names(w), w, sep = ":", collapse=", "))
    
    # make prediction based on the model - predict for all test/train
    prediction <- default.predict 
    if (!is.null (fit)) {
        loginfo("%s: predicting for '%s' all test/train obs.", by, nrow (data.x))
        prediction <- round (predict (fit, newdata = data.x))
    }
    
    return (prediction)
}

#
# train a challenger model.  this model uses GBM and performs centering
# and scaling of the 
#
challenger <- function (features, 
                        split.at, 
                        subset = "T==T", 
                        data.id = basename.only (opts$historyFile)) {
    
    challenger.cache <- sprintf ("%s-challenger", data.id)
    challenger <- cache (challenger.cache, {
        
        features[
            # include only those ATMs that pass the 'subset' expression
            eval (parse (text = subset)) & !is.na(usage),
            
            # train and fit a model
            list (
                trandate,
                usage,
                usage.hat = trainThenPredict (.BY, .SD, split.at, challenger.cache),
                model = "challenger"
            ),
            
            # training occurs independently for each ATM
            by = atm ] 
    })
}



