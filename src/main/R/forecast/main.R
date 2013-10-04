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

# if '--model' argument is 'gbm', then 'gbm.R' will be sourced
source(paste0(opts$model, ".R"))

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

# train and score the model by atm
scoreByAtm <- cache("scoreByAtm", {
    cash[, list(score=trainAndScore(.BY, .SD)), by=atm]
})

# calculate the total score over the test set
score <- sum(scoreByAtm$score, na.rm=T)
loginfo("Test Set Score --> %.1f points", score)








