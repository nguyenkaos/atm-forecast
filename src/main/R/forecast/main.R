library("plyr")
library("caret")
library("lubridate")
library("gbm")
library("gdata")
library("logging")

#source("../common/multicore.R")
source("../common/cache.R")
source("clean.R")
source("train.R")
source("score.R")
source("forecast.R")
source("gbm.R") 
#source("forest.R")

basicConfig(level=loglevels['INFO'])

# load, clean and cache the input data
cash <- cache("cash", { clean() })

# forecast each ATM 
july <- forecast(cash, by="atm", start="2013-07-01", end="2013-07-31")

# calculate the score for July
score <- with(july, sum(totalScore))
loginfo("total score for July is %.1f", score)
