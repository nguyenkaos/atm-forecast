#!/usr/bin/env Rscript

library("optparse")

# defines the options/arguments
getOptions <- function() {
    all_options <- list(
        make_option(c("--forecastOut"),
                    help    = "How far out to forecast in days [default: %default]",
                    default = 120),
        make_option(c("--splitAt"),
                    help    = "Date at which to split training vs test [default: %default]",
                    default = "2013-06-30"),
        make_option(c("-l", "--logLevel"),
                    help    = "Level of logging [default: %default]",
                    default = "INFO"),  
        make_option(c("--subset"),
                    help    = "An expression to identify a subset of ATMs to forecast [default: %default (all)]",
                    default = "T==T"),
        make_option(c("-d", "--dataDir"),
                    help    = "Directory containing the data files [default: %default]",
                    default = "../../resources"),
        make_option(c("--historyFile"), 
                    help    = "RDS file containing the ATM history [default: %default]",
                    default = "deposits-micro.rds")
    )
    opts <- parse_args (OptionParser (option_list = all_options))
}

# gather the command line options
opts <- getOptions()

# required libraries
library("plyr")
library("caret")
library("data.table")
library("lubridate")
library("logging")
library("foreach")

# other project sources
source("../common/cache.R")
source("../common/utils.R")
source("fetch.R")
source("train.R")
source("score.R")

basicConfig (level = loglevels [opts$logLevel])

# fetch and clean the input data
deposits <- cache("deposits-features", {
    fetch( history.file = opts$historyFile,
           forecast.to  = opts$forecastTo,
           data.dir     = opts$dataDir)
})

# train and score the model by atm
score.by.atm <- cache("deposit-score-by-atm", {
    deposits[ 
        eval (parse (text = opts$subset)), 
        c("usage.hat","ape","score") := trainAndScore (.BY, .SD, 
            method       = "gbm",
            split.at     = as.Date(opts$splitAt), 
            default      = expand.grid(.interaction.depth=2, .n.trees=50, .shrinkage=0.1), 
            verbose      = FALSE, 
            distribution = "poisson",
            cache.prefix = "deposits-fit"), 
        by = atm]
})

# extract the forecast
forecast <- score.by.atm [
    trandate >= today(), 
    list (
        atm       = atm, 
        trandate  = trandate,
        usage.hat = round(usage.hat)
    ), ]

# export the forecast to a csv file
filename <- sprintf("forecast-%s.csv", today())
write.csv(forecast, filename)
loginfo("forecasting complete and written to %s", filename)

# show the scores for july and august
score.by.atm [
    trandate>'2013-06-30' & trandate<'2013-09-01',
    list(
        score = sum(score, na.rm=T),
        mape  = mean(ape,na.rm=T),
        count = length(unique(atm))
    ), by = month(trandate) ]
