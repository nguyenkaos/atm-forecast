#!/usr/bin/env Rscript

# defines the options/arguments
library("optparse")
getOptions <- function() {
    
    # define all of the command-line options
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
    
    # parse the command-line
    opts <- parse_args (OptionParser (option_list = all_options))
}

# gather the command line options
opts <- getOptions()

#source("../common/parallel.R")
source("../common/cache.R")
source("../common/utils.R")
source("fetch.R")
source("score.R")
source("deposits-common.R")
source("deposits-models.R")

score.min.date <- "2013-08-01"
score.max.date <- "2013-09-30"

# initialization
options (warn = 0)
basicConfig (level = loglevels [opts$logLevel])
data.id <- basename.only (opts$historyFile)
split <- opts$splitAt

# generate features, build the champion and challengers, and combine them for scoring
f <- buildFeatures (split)
models <- combine (split, list (champion (f, split), challenger (f)))

# score by model
scoreBy (models, by = quote (list (model)), score.min.date, score.max.date) []

# should a detailed score be produced and exported?
if (opts$verbose) {
    
    # score by model (and export to disk this time)
    scoreBy (models,
             by          = quote (list (model)),
             min.date    = score.min.date,
             max.date    = score.max.date,
             export.file = sprintf("%s-score-by-model.csv", data.id))
    
    # score by atm
    scoreBy (models,
             by          = quote (list (model, atm)),
             min.date    = score.min.date,
             max.date    = score.max.date,
             export.file = sprintf("%s-score-by-atm.csv", data.id))
    
    # score by atm-day
    scoreBy (models, 
             by          = quote (list (model, atm, trandate)), 
             min.date    = score.min.date,
             max.date    = score.max.date,
             export.file = sprintf("%s-score-by-atm-date.csv", data.id))
}

# should the forecast be exported?
if (opts$export) {
    export.file <- sprintf("%s-challenger-forecast.csv", data.id)
    export (models, "challenger", data.id, min.date = score.min.date, export.file = export.file)
}

