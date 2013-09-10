library("plyr")
library("caret")
library("data.table")
library("lubridate")
library("gbm")
library("gdata")
library("logging")
library("foreach")

# init logging 
basicConfig(level=loglevels['INFO'])

source("../common/cache.R")
source("fetch.R")
source("train.R")
source("score.R")
source("utils.R")

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
    usageFile = args[1]
} else {
    usageFile = "usage-micro.rds" 
}
loginfo("using the '%s' data set", usageFile)

# fetch and clean the input data
cash <- fetch(usageFile=usageFile)

# train and score the model by atm
#julyScoreByAtm <- cash[, list(score=trainAndScore(.BY, .SD)), by=atm]
julyScoreByAtm <- cash[atm > median(atm), list(score=trainAndScore(.BY, .SD)), by=atm]

# calculate the scores for july; 2 points possible for each of 31 days
possibleScore <- nrow(julyScoreByAtm) * 2 * 31 
score <- sum(julyScoreByAtm$score, na.rm=T)
loginfo("July --> %.1f points or %.1f%% of points available", score, (score/possibleScore) * 100)
