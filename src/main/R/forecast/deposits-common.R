#
# create the feature set
#
buildFeatures <- function (history.file = opts$historyFile,
                           data.dir     = opts$dataDir, 
                           forecast.out = opts$forecastOut) {
    
    deposits.cache <- sprintf("%s-features", basename.only(history.file))
    deposits <- cache (deposits.cache, {
        
        # how far out should we forecast?
        forecast.to = today() + forecast.out
        
        # fetch usage history
        deposits <- fetch (history.file, forecast.to, data.dir)
        
        # generate the feature set
        dates (deposits)
        paydays (deposits, forecast.to)
        holidays (deposits, forecast.to,)
        localTrends (deposits)  
        globalTrends (deposits)
        lags (deposits)
        socialSecurity (deposits)
        
        # validate the feature set
        validate (deposits)
        
        loginfo("completed building feature set with '%s' obs and '%s' features", nrow(deposits), ncol(deposits))
        deposits
    })
}

#
# compare the champion and challenger models.combined
#
combine <- function (compare.start, list.of.models) {
    loginfo("combining '%s' models", length(list.of.models))
    
    # clean-up  all of the challengers data
    list.of.models <- lapply (list.of.models, function (m) m [trandate > compare.start] )
    
    # combine each model into a single data set for further comparison
    models.combined <- rbindlist (list.of.models)
    setkeyv (models.combined, c("model", "atm", "trandate"))
    
    return (models.combined)
}

# 
# produces a set of scores to compare multiple models.  the 'by' argument must
# be a quoted argument to avoid pre-mature evaluation.  if 'export.file' is
# provided the scores will be exported as a csv.
# 
scoreBy <- function (models, by, export.file = NA, min.date = -Inf, max.date = Inf) {
    
    # score the models
    scores <- models [
        trandate >= min.date & trandate <= max.date
        , list (
            err.total = sum(usage) - sum(usage.hat),
            err.abs   = sum (abs (usage - usage.hat)),
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
    
    scores
}

#
# export each of the challenger's forecast to a CSV.
#
export <- function (models, export.id, min.date = -Inf, max.date = Inf) {
    
    # export the forecast for each model
    forecast <- models [
        trandate >= min.date & trandate <= max.date, 
        list (
            atm       = atm, 
            trandate  = trandate,
            usage.hat = usage.hat
        ), 
        by = model]
    
    # export each forecast to a csv file
    forecast [
        , list(
            filename <- sprintf("%s-%s-forecast.csv", model, export.id),
            write.csv (forecast, filename),
            loginfo ("forecast exported to %s", filename)  
        ), by = model ]
}
