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
# jumps through some extra hoops like replacing NAs
#
findCorrelation <- function(x, use = "pairwise.complete.obs", ...) {
    
    # generate a correlation matrix with no NAs
    cor.mx <- cor (x, use = use)
    cor.mx [is.na(cor.mx)] <- 0
    
    # allow caret to do the hard part
    caret::findCorrelation(cor.mx)
}


#
# train the challenger and make a prediction for a specific ATM
#
trainThenPredict <- function (by,
                              data, 
                              data.id,
                              formula = usage ~ . -1 -train,
                              default.predict = 0.0,
                              max.prediction = 6000) {
    by <- by[[1]]
    
    train.index <- which (data[["train"]] == 1)
    
    # create the design matrix
    frame <- model.frame(formula, data, na.action = NULL)
    data.y <- model.response(frame)
    data.x <- model.matrix(formula, frame)
    
    # cache the trained model
    fit.cache <- sprintf ("%s-%s", data.id, by)
    fit <- cache (fit.cache, {
        loginfo("%s: pre-processing: [%s x %s]", by, nrow(data), ncol(data))
        
        # remove features that are highly correlated or with little/no variance
        data.x <- data.x[, -nearZeroVar(data.x)]
        data.x <- data.x[, -findCorrelation(data.x)]
        
        # additional pre-processing to prepare for training
        pre <- preProcess(data.x, method = c("center", "scale")) 
        data.x <- predict(pre, data.x)
        
        # split the training and test data
        train.x <- data.x [ train.index, ]
        train.y <- data.y [ train.index  ]
        test.x <- data.x [ -train.index, ]
        test.y <- data.y [ -train.index  ]
        
        # if no training data, or training response all 0s then don't train
        loginfo("%s: training: [%s x %s]", by, nrow(train.x), ncol(train.x))
        if (nrow (train.x) <= 0 || all(train.y == 0)) {
            return (NULL)
        }
        
        # define how tuning of the models should occur
        ctrl <- trainControl (
            method          = "repeatedcv",
            number          = 5, 
            repeats         = 3,
            returnResamp    = "none", 
            classProbs      = TRUE,
            returnData      = FALSE, 
            savePredictions = TRUE,
            allowParallel   = TRUE,
            #predictionBounds = c(0, max.prediction),
            index = createMultiFolds (train.y, k = 5, times = 1))
        
        # define each of the challenger models
        challengers.def <- list ( 
            list (x = train.x, y = train.y, trControl = ctrl, method = "gbm", verbose = F, keep.data = T),
            list (x = train.x, y = train.y, trControl = ctrl, method = "glmboost"),
            list (x = train.x, y = train.y, trControl = ctrl, method = "lasso"),
            list (x = train.x, y = train.y, trControl = ctrl, method = "leapForward", warn.dep = F)
            #list (x = train.x, y = train.y, trControl = ctrl, method = "glmnet"),
            #list (x = train.x, y = train.y, trControl = ctrl, method = "earth"),
            #list (x = train.x, y = train.y, trControl = ctrl, method = "svmRadial"), 
            #list (x = train.x, y = train.y, trControl = ctrl, method = "leapBackward", warn.dep = F),
            #list (x = train.x, y = train.y, trControl = ctrl, method = "nnet", trace = F)
        )
        
        # train each of the challengers; ignore any training failures
        challengers <- lapply (challengers.def, function(args) {
            tryCatch( do.call(train, args), 
                      error = function(e) logwarn("%s: training error encountered: %s", by, e))  
        })
        
        # remove any NULLs which would indicate failed training for one of the models
        challengers[ sapply(challengers, is.null)] <- NULL
        
        # create a greedy ensemble 
        fit <- caretEnsemble (challengers, iter = 1000L)
    })
    
    # log information about the trained model
    w <- sort(fit$weights, decreasing = TRUE)
    loginfo("%s: ensemble chosen with rmse: %.2f models: %s", by, fit$error,  paste (names(w), w, sep = ":", collapse=", "))
    
    # extract only the features used to train the model
    features <- fit$models[[1]]$finalModel$xNames
    data.x <- data.x[, features]
    
    # make a prediction - predict for all test/train
    loginfo("%s: predicting: [%s x %s]", by, nrow (data.x), ncol(data.x))
    prediction <- tryCatch ({
        round (predict (fit, newdata = data.x))
        
    }, error = function(e) {
        logerror("%s: prediction error; %s; using default: %s", by, e, default.predict)
        return (default.predict)
    })
}

#
# train a challenger model.  this model uses GBM and performs centering
# and scaling of the 
#
challenger <- function (features, 
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
                usage.hat = trainThenPredict (.BY, .SD, challenger.cache),
                model = "challenger"
            ),
            
            # training occurs independently for each ATM
            by = atm ] 
    })
}
