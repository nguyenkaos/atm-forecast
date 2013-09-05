
##################################################################
# Trains and scores a subset of data.  Returns the entire input data
# set with all features, along with the prediction and scoring
# metrics.  
##################################################################
trainAndScore <- function(data) {
    
    # cache the scored results for each ATM
    atm <- unique(as.character(data$atm))
    cache(sprintf("gbm-%s", atm), { 
        
        # train the model
        fit <- trainer(data, 
                       form=usage ~ dayOfYear + dayOfSemiYear + dayOfQuarter + 
                           dayOfWeek + weekOfMonth + weekOfYear + paydayN + holidayN + 
                           eventDistance + trandateN, 
                       p=ymd("2013-06-30"),
                       method="gbm", 
                       defaultTuneGrid = expand.grid(.interaction.depth=2, .n.trees=50, .shrinkage=0.1), 
                       verbose=F, 
                       distribution="poisson")
        
        # score the model
        scored <- score(data, fit)
        loginfo("total score for %s across train & test is %.1f", atm, sum(scored$score))
        scored
    })
}
