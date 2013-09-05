library("plyr")
library("caret")
library("lubridate")
library("gbm")
library("gdata")
library("logging")

source("../common/cache.R")
source("clean.R")
source("train.R")
source("score.R")

###
# to switch between prediction algorithms, source the
# appropriate file here
source("gbm.R") 
#source("forest.R")

###
# for parallel/multicore processing, source parallel.R
# to load the backend and set doParallel=T
#source("../common/parallel.R")
doParallel=F

# init logging 
basicConfig(level=loglevels['INFO'])

# fetch and clean the input data
cash <- cache("cash", { clean() })

# train and score the model by atm
scoreByAtm <- ddply(cash, "atm", trainAndScore, .parallel=doParallel)
saveRDS(scoreByAtm, "scoreByAtm.rds")  

# summarize the scores by day
scoreByDate <- ddply(scoreByAtm, ~trandate, summarise, totalScore=sum(score), .parallel=doParallel)
saveRDS(scoreByDate, "scoreByDate.rds")

# calculate the scores for july
july <- subset(scoreByDate, trandate>=as.Date("2013-07-01") & trandate<=as.Date("2013-07-31"))
loginfo("total score for July is %.1f", with(july, sum(totalScore)))
