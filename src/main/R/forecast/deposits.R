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
                     default = FALSE),
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

# initialization
basicConfig (level = loglevels [opts$logLevel])
data.id <- basename.only (opts$historyFile)

#
# create the feature set
#
features <- function (history.file = opts$historyFile,
                      data.dir     = opts$dataDir, 
                      forecast.out = opts$forecastOut) {
    
    deposits.cache <- sprintf("%s-features", basename.only(history.file))
    deposits <- cache (deposits.cache, {
        
        # how far out should we forecast?
        forecast.to = today() + forecast.out
        
        # fetch usage history
        deposits <- fetch (history.file, forecast.to, data.dir)
        
        # generate the feature set
        dates (deposits)
        paydays (deposits, forecast.to)
        holidays (deposits, forecast.to,)
        localTrends (deposits)  
        globalTrends (deposits)
        
        # validate the feature set
        #validate (deposits)
        deposits
    })
}

#
# train a challenger model
#
challenger <- function (features) {
    
    challenger.cache <- sprintf ("%s-challenger", data.id)
    challenger <- cache (challenger.cache, {
        
        features[ 
            # include only those ATMs that pass the 'subset' expression (optional)
            eval (parse (text = opts$subset)), 
            
            # train and fit a model
            `:=` (
                model = "challenger",
                usage.hat = trainAndPredict (
                    .BY, 
                    .SD, 
                    split.at        = as.Date (opts$splitAt), 
                    cache.prefix    = data.id,
                    
                    # define how the model will be tuned
                    train.control   = trainControl ( 
                        method        = "repeatedcv", 
                        number        = 5,
                        repeats       = 1,
                        returnData    = FALSE,
                        allowParallel = TRUE ),
                    
                    # what are we trying to predict?
                    formula         = usage ~ .,
                    default.predict = 0.0,
                    
                    # parameters specific to the training method
                    method          = "gbm",
                    preProcess      = c("center", "scale"),
                    verbose         = FALSE, 
                    distribution    = "poisson",
                    keep.data       = FALSE,
                    default.tune    = expand.grid ( .n.trees = 100, 
                                                    .shrinkage = 0.1,
                                                    .interaction.depth = 2))
            ),
            
            # training occurs independently for each ATM
            by = atm ] 
    })
}

#
# fetch the current champion model
#
champion <- function (feature.set, champion.file = "../../resources/deposits-champion.rds") {
    
    # fetch current champion's forecast 
    champion <- readRDS(champion.file)
    
    # TODO not sure this what i need
    setkeyv(feature.set, c("atm", "trandate"))
    
    # we are only interested in those atm-days in the feature set
    champion <- champion [ 
        feature.set[ trandate > compare.start & trandate < compare.end],
        list (
            usage,
            usage.hat,
            model = "champion"
        )] 
    
    # TEMP - rewrite any usage.hat NA's as 0 for now.  not sure why this is.
    champion [is.na(usage.hat), usage.hat := 0, ]
}

#
# compare the champion and challenger models
#
compare <- function(champion, challenger) {
    
    # clean-up the challenger data
    challenger <- challenger [
        trandate > compare.start & trandate < compare.end, 
        colnames(champion), 
        with = FALSE]
    
    # combine each into a single data set for further comparison
    models <- rbindlist (list (champion, challenger))
    setkeyv (models, c("model", "atm", "trandate"))
    
    return(models)
}

#
# generate a detailed score for each atm-day
#
score <- function (models) {
    
    # score each atm-day for the champion and challenger
    scores.daily <- models [
        , list (
            usage,
            usage.hat,
            model,
            err.abs    = abs(usage - usage.hat),
            ape        = ape (usage, usage.hat),
            rmse       = rmse (usage, usage.hat),
            points     = points (usage, usage.hat)
        ), by = list(atm, trandate)]
    
    # export the daily scores of both models
    export.file <- sprintf("%s-details-%s.csv", data.id, today())
    write.csv(scores.daily, export.file, row.names = FALSE)
    loginfo("daily scores exported to '%s'", export.file)
}

#
# summarize the differences between champion and challenger
#
summarize <- function (models) {
    
    # create a summary of the differences between the models
    models.summary <- models [
        , list (
            err.total = sum(usage) - sum(usage.hat),
            err.abs   = sum (abs (usage - usage.hat)),
            mape      = mape (usage, usage.hat),
            rmse      = rmse (usage, usage.hat),
            points    = sum (points (usage, usage.hat)),
            u05.ape   = between (ape (usage, usage.hat), 0.00, 0.05),
            u10.ape   = between (ape (usage, usage.hat), 0.05, 0.10),
            u20.ape   = between (ape (usage, usage.hat), 0.10, 0.20),
            over.ape  = between (ape (usage, usage.hat), 0.20, Inf),
            total.obs = length (usage),
            total.atm = length (unique (atm))
        ), by = list (model, month (trandate)) ]
    
    # export the summary
    export.file <- sprintf("%s-summary-%s.csv", data.id, today())
    write.csv(models.summary, export.file, row.names = FALSE)
    loginfo("summary exported to '%s'", export.file)
    
    print (models.summary)
}

#
# export the challenger's forecast 
#
export <- function (challenger) {
    
    # extract the forecast
    forecast <- challenger [
        trandate >= today(), 
        list (
            atm       = atm, 
            trandate  = trandate,
            usage.hat = round(usage.hat)
        ), ]
    
    # export the forecast to a csv file
    filename <- sprintf("%s-forecast-%s.csv", data.id, today())
    write.csv(forecast, filename)
    loginfo("forecast exported to %s", filename)
}

#
# main() effectively starts here
#

# the test period for comparison is August
compare.start <- "2013-07-31"
compare.end <- "2013-09-01"

# generate the feature set
f <- features()

# compare the champion and challenger models
models <- compare( champion(f), challenger (f))
summarize (models)

# should a detailed score be produced?
if (opts$verbose) {
    score (models)        
}

# should the forecast be exported?
if (opts$export) {
    export (challenger)
}

