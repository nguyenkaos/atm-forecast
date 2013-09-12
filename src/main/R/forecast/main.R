#!/usr/bin/env Rscript

# defines the options/arguments
library("optparse")
all_options <- list(
    make_option(c("-dir", "--dataDir"),
                help="Directory containing the data files [default: %default]",
                default="../../resources"),
    make_option(c("-uf", "--usageFile"), 
                help="RDS file containing the ATM usage data [default: %default]",
                default="usage-micro.rds"),
    make_option(c("-hf", "--holidaysFile"), 
                help="CSV file containing holidays data [default: %default]",
                default="holidays.csv"),
    make_option(c("-ef", "--eventsFile"), 
                help="CSV file containing events data [default: %default]",
                default="events.csv"),
    make_option(c("-pf", "--paydaysFile"), 
                help="CSV file containing pay days data [default: %default]",
                default="paydays.csv"),
    make_option(c("-log", "--logLevel"),
                help="Level of logging [default: %default]",
                default="INFO"),
    make_option(c("-atms", "--atms"),
                help="An expression identifying which ATMs to forecast [default: %default (all)]",
                default="T==T")
)
opts <- parse_args(OptionParser(option_list=all_options))

# required libraries
library("plyr")
library("caret")
library("data.table")
library("lubridate")
library("gbm")
library("gdata")
library("logging")
library("foreach")

# other project sources
source("../common/cache.R")
source("fetch.R")
source("train.R")
source("score.R")
source("utils.R")
source("gbm.R")                     # uncomment if using gbm
#source("forest.R")                 # uncomment if using random forest
#source("../common/parallel.R")     # uncomment for parallel/multicore processing

basicConfig(level=loglevels[opts$logLevel])

# fetch and clean the input data
cash <- fetch(usageFile=opts$usageFile, holidaysFile=opts$holidaysFile, eventsFile=opts$eventsFile, 
              paydaysFile=opts$paydaysFile, dataDir=opts$dataDir)

# train and score the model by atm
julyScoreByAtm <- cash[eval(parse(text=opts$atms)), list(score=trainAndScore(.BY, .SD)), by=atm]

# calculate the scores for july; 2 points possible for each of 31 days
possibleScore <- nrow(julyScoreByAtm) * 2 * 31 
score <- sum(julyScoreByAtm$score, na.rm=T)
loginfo("July --> %.1f points or %.1f%% of points available", score, (score/possibleScore) * 100)
