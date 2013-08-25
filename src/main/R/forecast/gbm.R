library("plyr")
library("caret")
library("lubridate")
library("gbm")
library("gdata")

source("utils/cache.R")
source("utils/clean.R")
source("utils/train.R")
source("utils/score.R")

# settings for parallel execution
#source("multicore.R")
parallel = FALSE

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
        fit <- trainer(data, form=usage ~ dayOfYear + dayOfSemiYear + dayOfQuarter + dayOfWeek + 
                           weekOfMonth + weekOfYear + paydayN + holidayN + eventDistance + trandateN, 
                       p=ymd("2013-06-30"),
                       method="gbm", defaultTuneGrid = expand.grid(.interaction.depth=2, .n.trees=50, .shrinkage=0.1), 
                       verbose=F, distribution="poisson")
        
        # score the model
        scored <- score(data, fit)
        return(scored)          
    })
}

gbmTrainer <- function() {
    # load, clean and cache the input data
    cash <- cache("cash", clean())
    
    # train and score the model by atm
    scoreByAtm <- ddply(cash, "atm", trainAndScore, .parallel=parallel)
    saveRDS(scoreByAtm, "./work/gbm-scoreByAtm.rds")  
    
    # summarize the scores by day
    scoreByDate <- ddply(scoreByAtm, ~trandate, summarise, totalScore=sum(score), .parallel=parallel)
    saveRDS(scoreByDate, "./work/gbm-scoreByDate.rds")
}







