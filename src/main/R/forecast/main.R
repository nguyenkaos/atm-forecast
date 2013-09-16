#!/usr/bin/env Rscript

# defines the options/arguments
library("optparse")
all_options <- list(
    make_option(c("-l", "--logLevel"),
                help="Level of logging [default: %default]",
                default="INFO"),
    make_option(c("-m", "--model"),
                help="The model to use for forecasting; gbm, forest [default: %default]",
                default="gbm"),  
    make_option(c("-a", "--atms"),
                help="An expression identifying which ATMs to forecast [default: %default (all)]",
                default="T==T"),
    make_option(c("-r", "--parallel"), 
                action="store_true",
                help="Run with a parallel backend enabled",
                default=F),
    make_option(c("-d", "--dataDir"),
                help="Directory containing the data files [default: %default]",
                default="../../resources"),
    make_option(c("-u", "--usageFile"), 
                help="RDS file containing the ATM usage data [default: %default]",
                default="usage-micro.rds"),
    make_option(c("-o", "--holidaysFile"), 
                help="CSV file containing holidays data [default: %default]",
                default="holidays.csv"),
    make_option(c("-e", "--eventsFile"), 
                help="CSV file containing events data [default: %default]",
                default="events.csv"),
    make_option(c("-p", "--paydaysFile"), 
                help="CSV file containing pay days data [default: %default]",
                default="paydays.csv")
)
opts <- parse_args(OptionParser(option_list=all_options))

# required libraries
library("plyr")
library("caret")
library("data.table")
library("lubridate")
library("gdata")
library("logging")
library("foreach")

# other project sources
source("../common/cache.R")
source("fetch.R")
source("train.R")
source("score.R")
source("utils.R")

# which model should be used?
if(opts$model=="gbm") {
    source("gbm.R")           
} else if(opts$model=="forest") { 
    source("forest.R") 
} else {
    stop(sprintf("invalid model: %s", opts$model))
}

# run multiple threads/cores
if(opts$parallel) {
    source("../common/parallel.R")   
}

basicConfig(level=loglevels[opts$logLevel])

# fetch and clean the input data
cash <- fetch(usageFile=opts$usageFile, 
              holidaysFile=opts$holidaysFile, 
              eventsFile=opts$eventsFile, 
              paydaysFile=opts$paydaysFile, 
              dataDir=opts$dataDir)


julyScoreByAtm <- cache("julyScoreByAtm", {
    # train and score the model by atm
    cash[eval(parse(text=opts$atms)), 
         list(score=trainAndScore(.BY, .SD)), 
         by=atm]
})

# calculate the scores for july; 2 points possible for each of 31 days
possibleScore <- nrow(julyScoreByAtm) * 2 * 31 
score <- sum(julyScoreByAtm$score, na.rm=T)
loginfo("July --> %.1f points or %.1f%% of points available", score, (score/possibleScore) * 100)
