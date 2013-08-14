library("plyr")
library("caret")
library("lubridate")
library("randomForest")
library("gdata")

source("utils/cache.R")
source("utils/clean.R")
source("utils/train.R")
source("utils/score.R")

# settings for parallel execution
#source("multicore.R")
parallel = FALSE

##################################################################
# Responsible for cacheing the tuning/training results from an 
# ATM to ensure that it is not unnecessarily re-calculated.
##################################################################
cacheByAtm <- function(data) {
    cacheFile = sprintf("forest-%s", unique(as.character(data$atm)))
    cache(cacheFile, trainAndScore, data)
}

##################################################################
# Trains and scores a set of data.  Returns the entire input data
# set with all features, along with the prediction and scoring
# metrics.
##################################################################
trainAndScore <- function(data) {
    
    # train the model
    fit <- trainer(data, 
                   form=usage ~ dayOfYear + dayOfSemiYear + dayOfQuarter + dayOfWeek + 
                       weekOfMonth + weekOfYear + paydayN + holidayN + eventDistance + trandateN, 
                   p=ymd("2013-06-30"),
                   method="rf", 
                   defaultTuneGrid=expand.grid(.mtry=max(floor(ncol(data)/3), 1)))
    
    # score the model
    scored <- score(data, fit)
    
    return(scored)
}

# load, clean and cache the input data
cash <- cache("cash", clean)

# train and score the model by atm
scoreByAtm <- ddply(cash, "atm", cacheByAtm, .parallel=parallel)
saveRDS(scoreByAtm, "./work/forest-scoreByAtm.rds")  

# summarize the scores by day
scoreByDate <- ddply(scoreByAtm, ~trandate, summarise, totalScore=sum(score), .parallel=parallel)
saveRDS(scoreByDate, "./work/forest-scoreByDate.rds")




