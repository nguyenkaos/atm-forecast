library("plyr")
library("caret")
library("lubridate")
library("gbm")
library("gdata")

source("multicore.R")
source("clean.R")
source("train.R")
source("score.R")

# create a working directory
dir.create(path="./work", showWarnings=FALSE)

pathToCash = "./work/cash.rds"
if(!file.exists(pathToCash)) {
  
  # load the raw input data
  withdrawals <- readRDS("../resources/withdrawals.rds")
  holidays <- read.csv("../resources/holidays.csv")
  events <- read.csv("../resources/events.csv")
  paydays <- read.csv("../resources/paydays.csv")
  
  # clean the data
  cash <- clean(withdrawals, holidays, paydays, events)
  saveRDS(cash, pathToCash) 
  
} else {
  
  # reuse the data that was previously cleaned
  cash <- readRDS(pathToCash)
}

# clean up memory
keep(cash)
gc()

trainAndScoreByAtm <- function(data) {
  
  scored <- NULL
  atm = unique(as.character(data$atm))
  atmFile = paste("./work/atm-", atm, ".rds", sep="")
  
  # the scores were already calculated for this ATM
  if(file.exists(atmFile)) {
    print(sprintf("loading existing scores from %s", atmFile))
    scored <- readRDS(atmFile)
    
    # train and score the ATM
  } else {
    print(sprintf("training... ATM=%s nrows=%.0f minDate=%s maxDate=%s\n", 
                  atm, 
                  nrow(data),
                  min(data$trandate),
                  max(data$trandate))) 
    
    # train the model
    fit <- trainer(data, 
                   form=usage ~ sin(dayOfYear*2*pi) + sin(dayOfSemiYear*2*pi) + sin(dayOfQuarter*2*pi) + dayOfWeek + weekOfMonth + weekOfYear + paydayN + holidayN + eventDistance, 
                   method="gbm", 
                   defaultTuneGrid = expand.grid(.interaction.depth=2, .n.trees=50, .shrinkage=0.1), 
                   p=ymd("2013-06-30"),
                   verbose=F, distribution="poisson")
    
    # score the model
    if(!is.null(fit)) {
      scored <- score(data, fit)
      
      # print diagnostics
      test <- subset(scored, isTest=TRUE)
      print(sprintf("test set results -> rmse = %.2f  r2 = %.2f  score = %.2f%%", 
                    RMSE(test$usageHat, test$usage, na.rm=T),
                    R2(test$usageHat, test$usage, na.rm=T),
                    100*sum(test$score, na.rm=T)/(nrow(test)*2)))
      
    } else {
      # not enough data to train a model
      scored <- data.frame()
      print(sprintf("not enough data to score ATM: %s", atm)) 
    }
  }
  
  # save off the scored data
  saveRDS(scored, paste("./work/atm-", atm, ".rds", sep=""))
  keep(scored)
  
  return(scored)
}

# train and score the model by atm
scoreByAtm <- ddply(cash, "atm", trainAndScoreByAtm, .parallel=FALSE)
saveRDS(scoreByAtm, "./work/scoreByAtm.rds")  
keep(scoreByAtm)

# view score by day
scoreByDate <- ddply(scoreByAtm, ~trandate, summarise, totalScore=sum(score), .parallel=FALSE)
saveRDS(scoreByDate, "./work/scoreByDate.rds")





