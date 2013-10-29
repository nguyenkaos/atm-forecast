#!/usr/bin/env Rscript

library("optparse")

# defines the options/arguments
getOptions <- function() {
    all_options <- list(
        make_option (c("--forecastOut"),
                     help    = "How far out to forecast in days [default: %default]",
                     default = 120),
        make_option (c("--splitAt"),
                     help    = "Date at which to split training vs test [default: %default]",
                     default = "2013-07-16"),
        make_option (c("-l", "--logLevel"),
                     help    = "Level of logging [default: %default]",
                     default = "INFO"),  
        make_option (c("--subset"),
                     help    = "An expression to identify a subset of ATMs to forecast [default: %default (all)]",
                     default = "T==T"),
        make_option (c("-d", "--dataDir"),
                     help    = "Directory containing the data files [default: %default]",
                     default = "../../resources"),
        make_option (c("--historyFile"), 
                     help    = "RDS file containing the ATM history [default: %default]",
                     default = "deposits-micro.rds"),
        make_option (c("e", "--export"),
                     help    = "Export the forecast [default: %default]",
                     default = FALSE,
                     action  = "store_true"),
        make_option (c("--verbose"),
                     help    = "The verbosity of the champion/challenger comparison [default: %default]",
                     default = FALSE,
                     action  = "store_true")
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
library("Metrics")

# other project sources
source("../common/cache.R")
source("../common/utils.R")
source("fetch.R")
source("train.R")
source("score.R")
source("deposits-common.R")
source("deposits-challengers.R")

# initialization
options (warn = 1)
basicConfig (level = loglevels [opts$logLevel])
split.at <- opts$splitAt
data.id <- basename.only (opts$historyFile)

# generate the features, build the champion and challengers, and combine them for scoring
f <- buildFeatures()
models <- combine( split.at, list (alpha (f, split.at),
                                   #beta (f, split.at),
                                   champion (f, split.at)))

# score by model
models <- models [is.finite(usage)]
scoreBy (models,
         by = quote (list (model)),
         export.file = sprintf("%s-score-by-model.csv", data.id))

# should a detailed score be produced?
if (opts$verbose) {
    
    # score the models by atm
    scoreBy (models,
             by = quote (list (model, atm)),
             export.file = sprintf("%s-score-by-atm.csv", data.id))
    
    # score the models by atm-date
    scoreBy (models, 
             by = quote (list (model, atm, trandate)), 
             export.file = sprintf("%s-score-by-atm-date.csv", data.id))
}

# should the forecast be exported?
if (opts$export) {
    export (models, data.id)
}

