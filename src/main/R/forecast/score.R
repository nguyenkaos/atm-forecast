library("caret")


############################################################################
# Scores a set of predictions made by a model's 'fit'.
############################################################################
score <- function(data, fit) {
    
    if(nrow(data) > 0 && !is.null(fit)) {
        # predict the training set 
        data$usage.hat <- predict(fit, newdata=data)
        data$mape <- mapply(mape, data$usage, data$usage.hat)
        data$score <- mapply(points, data$mape)
    } else {
        # not enough information to score
        data$usage.hat <- NA
        data$mape <- NA
        data$score <- NA
    }
    
    return(data)  
}

############################################################################
# Calculates the MAPE or "Mean Adjusted Percent Error" when given a single
# actual outcome and the predicted outcome.
############################################################################
mape <- function(actual, predict) {
    mape <- NA
    
    if(!is.na(actual))
        mape <- abs((actual - predict) / actual)
    
    return(mape)
}

############################################################################
# Calculates the number of competition points awarded based on the mape.
############################################################################
points <- function(mape) {
    score <- NA
    
    if(is.na(mape))
        score <- NA
    else if(mape <= 0.05) 
        score <- 2.0
    else if(mape <= 0.10)
        score <- 1.0
    else if(mape <= 0.20)
        score <- 0.5
    else
        score <- 0.0
    
    return(score)
}

############################################################################
# Logs the total score and percentage of possible from a scored data set.
############################################################################
logScore <- function(atm, scored) {
    score <- sum(scored$score)
    possible.score <- nrow(scored) * 2
    perc <- (score/possible.score) * 100
    loginfo("%s --> %.1f points or %.1f%% of points available", atm, score, perc)
}

