
##################################################################
# Trains and scores a subset of data.  Returns the entire input data
# set with all features, along with the prediction and scoring
# metrics.  
##################################################################
trainAndScore <- function(data) {
    atm <- unique(as.character(data$atm))
    
    # build and cache the fitted model
    fit <- cache(sprintf("fit-%s", atm), { 
        splitAt <- ymd("2013-07-01")
        
        # train the model
        fit <- trainer(data, 
                       #form=usage ~ .,
                       form=usage ~ 
                           dayOfYear + dayOfSemiYear + dayOfQuarter + dayOfWeek + 
                           weekOfMonth + weekOfYear + paydayN + holidayN + 
                           eventDistance + trandateN + 
                           woyMean + woyMin + woyMax + woySd + 
                           moyMean + moyMin + moyMax + moySd + 
                           dowMean + dowMin + dowMax + dowSd + 
                           womMean + womMin + womMax + womSd + 
                           qMean + qMin + qMax + qSd,
                       p=splitAt,
                       method="gbm", 
                       defaultTuneGrid = expand.grid(.interaction.depth=2, .n.trees=50, .shrinkage=0.1), 
                       verbose=F, 
                       distribution="poisson")
    })
    
    # score the model
    all <- score(data, fit)
    logScore(subset(all, !(trandateN %in% fit$trainingData$trandateN)))
    
    return(all)
}
