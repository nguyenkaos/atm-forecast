library("plyr")
library("caret")
library("lubridate")
library("gbm")
library("gdata")

source("../common/cache.R")
source("clean.R")
source("train.R")
source("score.R")

# settings for parallel execution
#source("multicore.R")
parallel = FALSE

##################################################################
# Trains and scores a subset of data.  Returns the entire input data
# set with all features, along with the prediction and scoring
# metrics.  
##################################################################
gbm.trainAndScore <- function(data) {
    
    # cache the scored results for each ATM
    atm <- unique(as.character(data$atm))
    cache(sprintf("gbm-%s", atm), { 
        
        # train the model
        fit <- forecast.trainer(data, 
                                form=usage ~ dayOfYear + dayOfSemiYear + dayOfQuarter + 
                                  dayOfWeek + weekOfMonth + weekOfYear + paydayN + holidayN + 
                                  eventDistance + trandateN, 
                       p=ymd("2013-06-30"),
                       method="gbm", defaultTuneGrid = expand.grid(.interaction.depth=2, .n.trees=50, .shrinkage=0.1), 
                       verbose=F, distribution="poisson")
        
        # score the model
        scored <- forecast.score(data, fit)
        return(scored)          
    })
}

#
# TODO - WANT TO BE ABLE TO PASS IN THE DATA TO TRAIN WITH?  AND THEN OUTPUT
# SOME MEASURE OF ACCURACY
#
gbm.forecast <- function(cash, by="atm") {
  
    # train and score the model by atm
    scoreByAtm <- ddply(cash, by, gbm.trainAndScore, .parallel=parallel)
    saveRDS(scoreByAtm, "gbm-scoreByAtm.rds")  
    
    # summarize the scores by day
    scoreByDate <- ddply(scoreByAtm, ~trandate, summarise, totalScore=sum(score), .parallel=parallel)
    saveRDS(scoreByDate, "gbm-scoreByDate.rds")
    
    # calculate the July score
    july <- subset(scoreByDate, 
                   trandate >= as.Date('2013-07-01','%Y-%m-%d') & 
                     trandate <= as.Date('2013-07-31','%Y-%m-%d'))

    return(july)
}







