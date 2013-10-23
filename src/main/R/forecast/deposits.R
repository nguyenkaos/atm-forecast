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
                    default = "2013-07-16"),
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
                    default = "deposits-micro.rds"),
        make_option(c("e", "--export"),
                    help    = "Export the forecast [default: %default]",
                    default = FALSE)
        
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

deposits.cache <- sprintf("%s-features", basename.only(opts$historyFile))
deposits <- cache(deposits.cache, {
    
    # fetch and clean the input data
    fetch( history.file = opts$historyFile,
           forecast.to  = today() + opts$forecastOut,
           data.dir     = opts$dataDir)
})

challenger.cache <- sprintf ("%s-challenger", basename.only(opts$historyFile))
challenger <- cache (challenger.cache, {
    
    fit.cache <- sprintf ("%s-fit", basename.only (opts$historyFile))
    deposits[ 
        # include only those ATMs that pass the 'subset' expression (optional)
        eval (parse (text = opts$subset)), 
        
        # train and fit a model
        `:=` (
            model = "challenger",
            usage.hat = trainAndPredict (
                .BY, 
                .SD, 
                method       = "gbm",
                split.at     = as.Date (opts$splitAt), 
                cache.prefix = fit.cache,
                #formula      = usage ~ ., # - I(atm) - I(holiday) - I(payday),
                
                # parameters specific to the training method
                verbose      = FALSE, 
                distribution = "poisson",
                keep.data    = FALSE,
                default      = expand.grid ( .n.trees = 100, 
                                             .shrinkage = 0.1,
                                             .interaction.depth = 2))
        ),
        
        # training occurs independently for each ATM
        by = atm] 
})

# compare the champion and challenger models; the test period is August
loginfo("comparing the champion to the challenger")
compare.start <- "2013-07-31"
compare.end <- "2013-09-01"
champion.file <- "../../resources/deposits-champion.rds"

# fetch current champion's forecast over the same set of ATM/dates as challenger
champion <- readRDS(champion.file)
champion <- champion [ 
    deposits[ trandate > compare.start & trandate < compare.end],
    list (
        usage,
        usage.hat,
        model = "champion"
    )] 

# clean-up the challenger data
challenger <- challenger [
    trandate > compare.start & trandate < compare.end, 
    colnames(champion), 
    with = FALSE]

# create a day-by-day comparison of the models
model.compare.details <- merge (
    subset (champion, select = c(atm, trandate, usage, usage.hat)), 
    subset (challenger, select = c(atm, trandate, usage.hat)), 
    by = c("atm", "trandate"),
    suffixes = c(".chmp",".chal")) 

# score each of the atm-days for the challenger
model.challenger.details <- challenger [
    , list (
        usage,
        usage.hat,
        model        = "challenger",
        points       = points (usage, usage.hat),
        mape         = mape (usage, usage.hat),
        mse          = mse (usage, usage.hat),
        rmse         = rmse (usage, usage.hat)
    ), by = list(atm, trandate)]

# score each of the atm-days for the champion
model.champion.details <- champion [
    , list (
        usage,
        usage.hat,
        model        = "champion",
        points       = sum (points (usage, usage.hat)),
        mape         = mape (usage, usage.hat),
        mse          = mse (usage, usage.hat),
        rmse         = rmse (usage, usage.hat)
    ), by = list(atm, trandate)]

# combine the champion and challenger for comparison over the test period
models <- rbindlist (list (champion, challenger))
model.compare.summary <- models [, list (
    points       = sum (points (usage, usage.hat)),
    mape         = mape (usage, usage.hat),
    mse          = mse (usage, usage.hat),
    rmse         = rmse (usage, usage.hat),
    under.05.ape = ape.between(usage, usage.hat, 0.00, 0.05),
    under.10.ape = ape.between(usage, usage.hat, 0.05, 0.10),
    under.20.ape = ape.between(usage, usage.hat, 0.10, 0.20),
    over.20.ape  = ape.between(usage, usage.hat, 0.20, Inf),
    total.obs    = length (usage),
    total.atm    = length (unique (atm))
), by = list(model, month(trandate)) ]
write.csv(model.compare.summary, sprintf("model-comparison-%s.csv", today()), row.names = T)

# should the forecast be exported?
if(opts$export) {
    
    # extract the forecast
    forecast <- challenger [
        trandate >= today(), 
        list (
            atm       = atm, 
            trandate  = trandate,
            usage.hat = round(usage.hat)
        ), ]
    
    # export the forecast to a csv file
    filename <- sprintf("forecast-%s.csv", today())
    write.csv(forecast, filename)
    loginfo("forecasting exported to %s", filename)
}

# output results
model.challenger.details[]
model.champion.details[]
model.compare.details[]
model.compare.summary[]
