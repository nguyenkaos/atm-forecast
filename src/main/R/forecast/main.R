library("plyr")
library("caret")
library("lubridate")
library("gbm")
library("gdata")
library("logging")

# init logging 
basicConfig(level=loglevels['INFO'])

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

# specify the "usage" data set to use
args <- commandArgs(trailingOnly=T)
if(length(args) > 0) {
    usage = args[1]
} else {
    usage = "usage-micro.rds" 
}
loginfo("using the '%s' data set", usage)

# fetch and clean the input data
cash <- cache("cash", { clean(usage=usage) })

# train and score the model by atm
scoreByAtm <- ddply(cash, "atm", trainAndScore, .parallel=doParallel)
saveRDS(scoreByAtm, "scoreByAtm.rds")  

# summarize the scores by day
scoreByDate <- ddply(scoreByAtm, ~trandate, summarise, score=sum(score), .parallel=doParallel)
saveRDS(scoreByDate, "scoreByDate.rds")

# calculate the scores for july
july <- subset(scoreByDate, trandate>=as.Date("2013-07-01") & trandate<=as.Date("2013-07-31"))
score <- sum(july$score)
possibleScore <- nrow(july) * length(unique(cash$atm)) * 2
loginfo("July --> %.1f points or %.1f%% of points available", score, (score/possibleScore) * 100)

