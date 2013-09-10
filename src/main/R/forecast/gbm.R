
##################################################################
# Trains and scores a subset of data.  Returns the entire input data
# set with all features, along with the prediction and scoring
# metrics.  
##################################################################
trainAndScore <- function(by, data) {
    atm <- by$atm
    
    # build and cache the fitted model
    fit <- cache(sprintf("fit-%s", atm), { 
        splitAt <- ymd("2013-07-01")
        
        # train the model
        fit <- trainer(data, 
                       form=usage ~ 
                           # date related features
                           trandateN + dayOfMonth + dayOfYear + dayOfSemiYear + dayOfQuarter + 
                           dayOfWeek + weekOfMonth + weekOfYear + quarter + monthOfYear + 
                           
                           # paydays, holidays, events
                           paydayN + holidayN + eventDistance +
                           
                           # usage trends specific to the ATM
                           woyMean + woyMin + woyMax + woySd + 
                           moyMean + moyMin + moyMax + moySd + 
                           dowMean + dowMin + dowMax + dowSd + 
                           womMean + womMin + womMax + womSd + 
                           quaMean + quaMin + quaMax + quaSd +
                           holMean + holMin + holMax + holSd +
                           payMean + payMin + payMax + paySd +
                           
                           # usage trends across all ATMs
                           woyAllMean + woyAllMin + woyAllMax + woyAllSd + 
                           moyAllMean + moyAllMin + moyAllMax + moyAllSd + 
                           dowAllMean + dowAllMin + dowAllMax + dowAllSd +  
                           womAllMean + womAllMin + womAllMax + womAllSd + 
                           quaAllMean + quaAllMin + quaAllMax + quaAllSd + 
                           holAllMean + holAllMin + holAllMax + holAllSd +  
                           payAllMean + payAllMin + payAllMax + payAllSd,
                       p=splitAt,
                       method="gbm", 
                       defaultTuneGrid = expand.grid(.interaction.depth=2, .n.trees=50, .shrinkage=0.1), 
                       verbose=F, 
                       distribution="poisson")
    })
    
    # score the model
    all <- score(data, fit)
    logScore(subset(all, !(trandateN %in% fit$trainingData$trandateN)))
    
    # return the score for July
    july <- subset(all, trandate>=as.Date("2013-07-01"))
    scoreInJuly <- sum(july$score, na.rm=T)
    return(scoreInJuly)
}
