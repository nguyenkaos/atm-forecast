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
                    default = "2013-07-31"),
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
           forecast.to  = today() + opts$forecastOut,
           data.dir     = opts$dataDir)
})

# train and score the model by atm
challenger <- cache("deposits-challenger", {
    deposits[ 
        eval (parse (text = opts$subset)), 
        c("usage.hat", "pe", "ape", "score") := 
            trainAndScore (.BY, .SD, 
                           method       = "gbm",
                           split.at     = as.Date (opts$splitAt), 
                           default      = expand.grid (.interaction.depth=2, .n.trees=50, .shrinkage=0.1), 
                           verbose      = FALSE, 
                           distribution = "poisson",
                           cache.prefix = "deposits-fit"), 
        by = atm] 
})

# compare the champion and challenger models
models.compare <- cache("deposits-model-compare", {
    
    # the test period is August
    compare.start <- "2013-07-31"
    compare.end <- "2013-09-01"
    compare.atms <- unique(deposits$atm)
    
    # score the existing champion
    champion <- readRDS("../../resources/deposits-champion.rds")
    champion <- champion [
        atm %in% compare.atms & trandate > compare.start & trandate < compare.end, 
        list (
            usage,
            usage.hat,
            model = "champion",
            pe    = pe (usage, usage.hat),
            ape   = ape (usage, usage.hat),
            score = points (ape (usage, usage.hat))
        ), by = list(atm, trandate)]
    
    # combine the champion and challenger for comparison
    challenger[, model := "challenger",]
    challenger <- challenger[, colnames(champion), with=FALSE]
    models <- rbindlist (list (champion, challenger))
    
    # summarize over the test period
    models.compare <- models [
        trandate > compare.start & trandate < compare.end,
        list(
            mape         = mean(ape, na.rm=T),
            under.5.ape  = length( ape [ape <= 0.05]),
            under.10.ape = length( ape [ape <= 0.10 & ape > 0.05]),
            under.20.ape = length( ape [ape <= 0.20 & ape > 0.10]),
            over.20.ape  = length( ape [ape >  0.20]),
            total.obs    = length( ape),
            total.atm    = length( unique(atm))
        ), by = list(model, month(trandate)) ]
})

models.compare

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
