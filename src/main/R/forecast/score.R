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


