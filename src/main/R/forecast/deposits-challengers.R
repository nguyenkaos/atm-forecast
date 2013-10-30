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
        features [ trandate > split.at & is.finite(usage) ],
        list (
            usage,
            usage.hat,
            model = "champion"
        )] 
    
    champion <- champion [ is.finite(usage.hat) ]
}

#
# train a challenger model.  this model uses GBM and performs centering
# and scaling of the 
#
alpha <- function (features, 
                   split.at, 
                   subset = "T==T", 
                   challenger.model = "alpha",
                   data.id = basename.only (opts$historyFile)) {
    
    challenger.cache <- sprintf ("%s-%s", data.id, challenger.model)
    challenger <- cache (challenger.cache, {
        
        # define how the model will be tuned/trained
        train.control <- trainControl (
            method        = "repeatedcv", 
            number        = 5,
            repeats       = 1,
            returnData    = FALSE,
            allowParallel = TRUE )
        
        features[
            # include only those ATMs that pass the 'subset' expression
            eval (parse (text = subset)) & !is.na(usage),
            
            # train and fit a model
            list (
                trandate,
                usage,
                usage.hat = trainAndPredict (
                    formula         = usage ~ .,
                    data            = .SD,
                    by              = .BY, 
                    split.at        = as.Date (split.at), 
                    cache.prefix    = sprintf ("%s-%s", data.id, challenger.model),
                    default.predict = 0.0,
                    train.control   = train.control,
                    method          = "gbm",
                    preProcess      = c("center", "scale"),
                    distribution    = "poisson",
                    verbose         = FALSE, 
                    keep.data       = FALSE
                ),
                model = challenger.model
            ),
            
            # training occurs independently for each ATM
            by = atm ] 
    })
}
