library("plyr")
library("caret")
library("lubridate")
#library("randomForest")

source("multicore.R")
source("clean.R")
source("train.R")
source("score.R")

pathToCash = "../resources/cash.rds"
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

trainAndScoreByAtm <- function(data) {
  print(sprintf("about to train... ATM=%s nrows=%.0f minDate=%s maxDate=%s\n", 
                unique(as.character(data$atm)), 
                nrow(data),
                min(data$trandate),
                max(data$trandate))) 
  
  # train and score the data
  fit <- trainer(data, 
                 form=usage ~ sin(dayOfYear*2*pi) + sin(dayOfSemiYear*2*pi) + sin(dayOfQuarter*2*pi) + dayOfWeek + weekOfMonth + weekOfYear + paydayN + holidayN + eventDistance, 
                 method="rf", 
                 defaultTuneGrid=expand.grid(.mtry=), 
                 p=ymd("2013-05-15"))
  
  if(exists("fit")) {
    scored <- score(data, fit)
    
    # print diagnostics
    test <- subset(scored, isTest=TRUE)
    print(sprintf("test set results -> rmse = %.2f  r2 = %.2f  score = %.2f%%", 
                  RMSE(test$usageHat, test$usage, na.rm=T),
                  R2(test$usageHat, test$usage, na.rm=T),
                  100*sum(test$score, na.rm=T)/(nrow(test)*2)))
  } else {
    scored <- data.frame()
    print(sprintf("not enough data to score ATM: %s", unique(as.character(cash$atm)))) 
  }
  
  return(scored)
}
  
# train and score the model by atm
scoreByAtm <- ddply(cash, "atm", trainAndScoreByAtm, .progress="text", .parallel=F)
saveRDS(scoreByAtm, "scoreByAtm.rds")  

# view score by day
scoreByDate <- ddply(scoreByAtm, ~trandate, summarise, totalScore=sum(score))
saveRDS(scoreByDate, "scoreByDate.rds")






