#!/usr/bin/env Rscript

# required libraries
library("plyr", quietly=T, warn.conflicts=F)
library("caret", quietly=T, warn.conflicts=F)
library("data.table", quietly=T, warn.conflicts=F)
library("lubridate", quietly=T, warn.conflicts=F)
library("logging", quietly=T, warn.conflicts=F)
library("foreach", quietly=T, warn.conflicts=F)

# other project sources
source("options.R")
source("../common/cache.R")
source("fetch.R")
source("train.R")
source("score.R")
source("utils.R")

# gather the command line options
opts <- getOptions()
basicConfig(level=loglevels[opts$logLevel])

# run multiple threads/cores
if(opts$parallel) {
    source("../common/parallel.R")   
}

# fetch and clean the input data
cash <- fetch(forecast.to   = today() + opts$forecastOut,
              usage.file    = opts$usageFile, 
              holidays.file = opts$holidaysFile, 
              events.file   = opts$eventsFile, 
              paydays.file  = opts$paydaysFile, 
              data.dir      = opts$dataDir)

# an expression can be provided to exclude certain ATMs; ex 'atm<median(atm)'
subset.expr <- parse(text = opts$subset)

# train and score the model by atm
score.by.atm <- cache("gbm-score-by-atm", {
    cash[ eval(subset.expr), 
          c("usage.hat","mape","score") := trainAndScore(
              .BY, .SD, 
              method = "gbm",
              split.at = as.Date(opts$splitAt), 
              default = expand.grid(.interaction.depth=2, .n.trees=50, .shrinkage=0.1), 
              verbose = F, 
              distribution = "poisson"), 
          by=atm]
})

# clean up the forecast itself
forecast <- subset(score.by.atm, trandate >= today(), select = c(atm,trandate,usage.hat))
forecast <- within(forecast, {
    usage.hat <- sapply(usage.hat, round)
})

# export the forecast to a csv file
filename <- sprintf("forecast-%s.csv", today())
write.csv(forecast, filename)
loginfo("forecasting complete and written to %s", filename)

# show the scores for july and august
julyAndAugust <- subset(score.by.atm, trandate>'2013-06-30' & trandate<'2013-09-01')
julyAndAugust[, list(score=sum(score, na.rm=T), 
                     count=length(unique(atm))), 
              by=month(trandate)]







