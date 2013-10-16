library("caret")

#
# Scores a set of predictions made by a model's 'fit'.
#
score <- function(data, fit) {
    
    if(nrow(data) > 0 && !is.null(fit)) {
        # predict the training set 
        data$usage.hat <- predict(fit, newdata=data)
        data$ape <- mapply(ape, data$usage, data$usage.hat)
        data$score <- mapply(points, data$ape)
    } else {
        # not enough information to score
        data$usage.hat <- NA
        data$ape <- NA
        data$score <- NA
    }
    
    return(data)  
}

#
# Calculates the APE or "Adjusted Percent Error" when given a single
# actual outcome and the predicted outcome.
#
ape <- function(actual, predict) {
    ape <- NA
    
    if(!is.na(actual))
        ape <- abs((actual - predict) / (actual+1))
    
    return(ape)
}

#
# Calculates the number of competition points awarded based on the ape.
#
points <- function(ape) {
    score <- NA
    
    if(is.na(ape))
        score <- NA
    else if(ape <= 0.05) 
        score <- 2.0
    else if(ape <= 0.10)
        score <- 1.0
    else if(ape <= 0.20)
        score <- 0.5
    else
        score <- 0.0
    
    return(score)
}

#
# Logs the total score and percentage of possible from a scored data set.
#
logScore <- function(atm, scored) {
    score <- sum(scored$score)
    possible.score <- nrow(scored) * 2
    perc <- (score/possible.score) * 100
    loginfo("%s --> %.1f points or %.1f%% of points available", atm, score, perc)
}

