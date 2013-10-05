#!/usr/bin/env Rscript

# defines the options/arguments
library("optparse")
all_options <- list(
    make_option(c("--forecastOut"),
                help="How far out to forecast in days.",
                default=120),
    make_option(c("--splitAt"),
                help="Date at which to split data into training vs test [default: %default]",
                default="2013-08-15"),
    make_option(c("-l", "--logLevel"),
                help="Level of logging [default: %default]",
                default="INFO"),
    make_option(c("-m", "--model"),
                help="The model to use for forecasting; gbm, forest [default: %default]",
                default="gbm"),  
    make_option(c("--subset"),
                help="An expression to identify a subset of ATMs to forecast [default: %default (all)]",
                default="T==T"),
    make_option(c("--parallel"), 
                action="store_true",
                help="Run with a parallel backend enabled",
                default=F),
    make_option(c("-d", "--dataDir"),
                help="Directory containing the data files [default: %default]",
                default="../../resources"),
    make_option(c("--usageFile"), 
                help="RDS file containing the ATM usage data [default: %default]",
                default="usage-micro.rds"),
    make_option(c("--holidaysFile"), 
                help="CSV file containing holidays data [default: %default]",
                default="holidays.csv"),
    make_option(c("--eventsFile"), 
                help="CSV file containing events data [default: %default]",
                default="events.csv"),
    make_option(c("--paydaysFile"), 
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
cash <- fetch(forecastTo   = today() + opts$forecastOut,
              usageFile    = opts$usageFile, 
              holidaysFile = opts$holidaysFile, 
              eventsFile   = opts$eventsFile, 
              paydaysFile  = opts$paydaysFile, 
              dataDir      = opts$dataDir)

# a subset expression can be provided to limit the number of ATMs that will be forecasted
subsetExpr <- parse(text=opts$subset)

# train and score the model by atm
scoreByAtm <- cache("scoreByAtm", {
    cash[eval(subsetExpr), 
         c("usageHat","mape","score"):=trainAndScore(.BY, .SD, splitAt=opts$splitAt), 
         by=atm]
})

# show a small portion of the resulting scores by atm
scoreByAtm <- subset(scoreByAtm, select=c(atm,trandate,usage,usageHat,mape,score))
head(scoreByAtm)
loginfo("forecasting complete")









