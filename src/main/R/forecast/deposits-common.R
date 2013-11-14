library("data.table")
library("logging")

#
# create the feature set
#
buildFeatures <- function (split.at     = opts$splitAt,
                           history.file = opts$historyFile,
                           data.dir     = opts$dataDir, 
                           forecast.out = opts$forecastOut) {
    
    deposits.cache <- sprintf("%s-features", basename.only(history.file))
    deposits <- cache (deposits.cache, {
        
        # fetch the deposits history
        forecast.to = today() + forecast.out
        deposits <- fetch (history.file, forecast.to, data.dir)
        
        # impute usage where faults may have affected the "actuals"
        # faults (deposits)
        
        # split and mark test versus training data
        split.at <- as.Date(split.at)
        train.index <- which ( deposits[["trandate"]] < split.at )
        deposits [trandate <  split.at, train := 1, ]
        deposits [trandate >= split.at, train := 0, ]
        
        # remove 'usage' from test data to prevent accidental 'bleed-through'
        deposits.usage <- deposits[, list(atm, trandate, usage.saved = usage)]
        deposits [train == 0, usage := NA, ]
        
        # generate the feature set
        dates (deposits)
        paydays (deposits)
        holidays (deposits)
        socialSecurity (deposits)
        rollingTrends (deposits)
        recentHistory (deposits)
        
        # add the 'usage' back into the feature set
        setkeyv (deposits, c("atm", "trandate"))
        setkeyv (deposits.usage, c("atm", "trandate"))
        deposits [deposits.usage, usage := usage.saved]
        
        # validate the feature set
        validate (deposits)
        
        loginfo("completed building feature set: [%s x %s]", nrow(deposits), ncol(deposits))
        deposits
    })
}

#
# compare the champion and challenger models.combined
#
combine <- function (compare.start, list.of.models) {
    logdebug("combining '%s' models", length(list.of.models))
    
    # clean-up  all of the challengers data
    list.of.models <- lapply (list.of.models, function (m) m [trandate > compare.start] )
    
    # combine each model into a single data set for further comparison
    models.combined <- rbindlist (list.of.models)
    models.combined [, model := factor(model), ]
    setkeyv (models.combined, c("model", "atm", "trandate"))
    
    loginfo("combined '%s' models: [%s x %s]", length(list.of.models), nrow(models.combined), ncol(models.combined))
    return (models.combined)
}

# 
# produces a set of scores to compare multiple models.  the 'by' argument must
# be a quoted argument to avoid pre-mature evaluation.  if 'export.file' is
# provided the scores will be exported as a csv.
# 
scoreBy <- function (models, by, min.date = -Inf, max.date = Inf, export.file = NA) {
    
    # score the models
    scores <- models [
        trandate >= min.date & trandate <= max.date
        , list (
            usage     = sum (usage),
            usage.hat = sum (usage.hat),
            mape      = mape (usage, usage.hat),
            rmse      = rmse (usage, usage.hat),
            points    = sum (points (usage, usage.hat)),
            u05.ape   = between (ape (usage, usage.hat), 0.00, 0.05),
            u10.ape   = between (ape (usage, usage.hat), 0.05, 0.10),
            u15.ape   = between (ape (usage, usage.hat), 0.10, 0.15),
            u20.ape   = between (ape (usage, usage.hat), 0.15, 0.20),
            over.ape  = between (ape (usage, usage.hat), 0.20, Inf),
            total.obs = length (usage),
            total.atm = length (unique (atm))
        ), by = eval (by)]
    
    # export the scores
    if (!is.na (export.file)) {
        write.csv (scores, export.file, row.names = FALSE)
        loginfo ("scores exported to '%s'", export.file)
    }
    
    logdebug("scored by (%s): [%s x %s]", by, nrow(scores), ncol(scores))
    scores
}

#
# export each of the challenger's forecast to a CSV.
#
export <- function (models, model.name, data.id, min.date = -Inf, max.date = Inf, export.file = NA) {
    
    # export the forecast for each model
    forecast <- models [
        model == model.name & trandate >= min.date & trandate <= max.date, 
        list (
            atm       = atm, 
            trandate  = trandate,
            usage.hat = usage.hat
        )]
    
    # export each forecast to a csv file
    if (!is.na(export.file)) {
        write.csv (forecast, export.file, row.names = F)
        loginfo ("forecast exported to %s", export.file)   
    }
    
    return (forecast)
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

