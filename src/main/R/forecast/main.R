#!/usr/bin/env Rscript

# gather the command line options
source("options.R")
opts <- getOptions()

# required libraries
library("plyr", quietly=T)
library("caret", quietly=T)
library("data.table", quietly=T)
library("lubridate", quietly=T)
library("logging", quietly=T)
library("foreach", quietly=T)

# other project sources
source("../common/cache.R")
source("fetch.R")
source("train.R")
source("score.R")
source("utils.R")

basicConfig(level=loglevels[opts$logLevel])

# run multiple threads/cores
if(opts$parallel) 
    source("../common/parallel.R")   

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
score.by.atm <- cash[ 
    eval(subset.expr), 
    c("usage.hat","mape","score") := trainAndScore(
        .BY, .SD, 
        method = "gbm",
        split.at = as.Date(opts$splitAt), 
        default = expand.grid(.interaction.depth=2, .n.trees=50, .shrinkage=0.1), 
        verbose = F, 
        distribution = "poisson"), 
    by=atm]
saveRDS(score.by.atm, ".cache/score-by-atm.rds")

# extract the forecast
forecast <- score.by.atm [trandate >= today(), 
                          list (
                              atm = atm, 
                              trandate = trandate,
                              usage.hat = round(usage.hat)
                          ), ]

# export the forecast to a csv file
filename <- sprintf("forecast-%s.csv", today())
write.csv(forecast, filename)
loginfo("forecasting complete and written to %s", filename)

# show the scores for july and august
score.by.atm [trandate>'2013-06-30' & trandate<'2013-09-01',
              list(
                  score=sum(score, na.rm=T), 
                  count=length(unique(atm))
              ), by=month(trandate)]
