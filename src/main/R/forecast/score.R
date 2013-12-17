library("caret")
library("Metrics")

#
# forecasts the percent error
#
pe <- function (actual, predict) {
    (predict - actual) / (actual+1)
}

#
# Calculates the APE or "Adjusted Percent Error" when given a single
# actual outcome and the predicted outcome.
#
ape <- function (actual, predict) { 
    abs (pe (actual, predict)) 
}

#
# calculates the mean absolute percent error aka mape
#
mape <- function (actual, predict) {
    mean (ape (actual, predict), na.rm = T)
}

#
# calculate the symmetric mean absolute percent error aka smape. with this
# version of smape, the result will always be between 0% and 100%.
#
smape <- function (actual, predict, na.rm = T) {
    (1 / length (actual)) * sum (abs (predict - actual) / (actual + predict), 
                                 na.rm = na.rm)
}

rmse <- function (actual, predict, na.rm = T) {
    caret::RMSE (actual, predict, na.rm = na.rm)
}

r2 <- function (actual, predict, na.rm = T) {
    caret::R2 (actual, predict, na.rm = na.rm)
}

#
# calculates the number of competition points awarded based on the actual
# and predicted values.
#
points <- function (actual, predict) {
    points.ape (ape (actual, predict))
}

#
# Calculates the number of competition points awarded based on the ape.
#
points.ape <- function (ape) {
    points <- rep (NA, length (ape))
    
    # within 5% gets 2 points
    points[ ape <= 0.05] <- 2.0
    
    # within 10% gets 1 point
    points[ ape <= 0.10 & ape > 0.05] <- 1.0
    
    # within 20% gets 0.5 points
    points[ ape <= 0.20 & ape > 0.10] <- 0.5
    
    # anything else gets ya' nothing
    points[ ape > 0.20] <- 0.0
    points[ is.na(ape)] <- 0.0
    
    return(points)
}

#
# counts the number of predictions with an APE between 
# lower and upper
#
between <- function (values, lower, upper) {
    sum (values <= upper & values > lower, na.rm = TRUE)
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
        ,list (
            usage     = sum (usage),
            usage.hat = sum (usage.hat),
            mape      = mape (usage, usage.hat),
            smape     = smape (usage, usage.hat),
            rmse      = rmse (usage, usage.hat),
            r2        = r2 (usage, usage.hat),
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
        list (atm, trandate, usage.hat) ]
    
    # export each forecast to a csv file
    if (!is.na(export.file)) {
        write.csv (forecast, export.file, row.names = F)
        loginfo ("forecast exported to %s", export.file)   
    }
    
    return (forecast)
}
