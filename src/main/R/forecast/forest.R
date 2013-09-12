library("randomForest")

##################################################################
# Trains and scores a set of data.  Returns the entire input data
# set with all features, along with the prediction and scoring
# metrics.  
##################################################################
trainAndScore <- function(data) {
    
    # cache the scored results for each ATM
    atm <- unique(as.character(data$atm))
    cache(sprintf("forest-%s", atm), { 
        
        # train the model
        fit <- trainer(data, form=usage ~ dayOfYear + dayOfSemiYear + dayOfQuarter + dayOfWeek + 
                           weekOfMonth + weekOfYear + paydayN + holidayN + eventDistance + trandateN, 
                       p=ymd("2013-06-30"),
                       method="rf", 
                       defaultTuneGrid=expand.grid(.mtry=max(floor(ncol(data)/3), 1)))
        
        # score the model
        scored <- score(data, fit)         
        loginfo("total score for %s across train & test is %.1f", atm, sum(scored$score))
        scored
    })
}
