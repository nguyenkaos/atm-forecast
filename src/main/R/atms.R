library("plyr")
library("caret")
library("lubridate")
library("randomForest")

source("multicore.R")
source("clean.R")
source("train.R")
source("score.R")

setwd("~/batchR/src/main/R")

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

trainByAtm <- function(data) {
  
  # train and score the data
  fit <- trainer(data, 
                 f=usage ~ sin(dayOfYear*2*pi) + sin(dayOfSemiYear*2*pi) + sin(dayOfQuarter*2*pi) + dayOfWeek + weekOfMonth + weekOfYear + paydayN + holidayN + eventDistance, 
                 method="rf", 
                 defaultTuneGrid=expand.grid(.mtry=) , 
                 p=ymd("2013-05-15"))
  scored <- score(data, fit)
  
  # print diagnostics
  test <- subset(scored, isTest=TRUE)
  print(sprintf("test set results -> rmse = %.2f  r2 = %.2f  score = %.2f%%", 
                RMSE(test$usageHat, test$usage, na.rm=T),
                R2(test$usageHat, test$usage, na.rm=T),
                100*sum(test$score, na.rm=T)/(nrow(test)*2)))
  
  return(scored)
}
  
# train and score the model by atm
results <- ddply(cash, "atm", trainByAtm, .progress="text", .parallel=F)
saveRDS(results, paste("results", format(Sys.time(), "%Y%m%d-%H%M%S"), "rds", sep = "."))  






