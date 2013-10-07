#!/usr/bin/env Rscript

# required libraries
library("plyr")
library("caret")
library("data.table")
library("lubridate")
library("logging")
library("foreach")

# other project sources
source("options.R")
source("../common/cache.R")
source("fetch.R")
source("train.R")
source("score.R")
source("utils.R")

# gather the command line options
opts <- options()
basicConfig(level=loglevels[opts$logLevel])

# run multiple threads/cores
if(opts$parallel) {
    source("../common/parallel.R")   
}

# fetch and clean the input data
cash <- fetch(forecastTo   = today() + opts$forecastOut,
              usageFile    = opts$usageFile, 
              holidaysFile = opts$holidaysFile, 
              eventsFile   = opts$eventsFile, 
              paydaysFile  = opts$paydaysFile, 
              dataDir      = opts$dataDir)

# a subset expression can be provided to limit the number of ATMs that will be forecasted
subsetExpr <- parse(text = opts$subset)

# train and score the model by atm
scoreByAtm <- cache("gbm-score-by-atm", {
    cash[ eval(subsetExpr), 
          c("usageHat","mape","score") 
          := trainAndScore(.BY, .SD, 
                           method = "gbm",
                           splitAt = as.Date(opts$splitAt), 
                           default = expand.grid(.interaction.depth=2, .n.trees=50, .shrinkage=0.1), 
                           verbose = F, 
                           distribution = "poisson"), 
          by=atm]
})

# export the forecast to a csv file
forecast <- subset(scoreByAtm, trandate >= today(), select = c(atm,trandate,usageHat))
write.csv(forecast, sprintf("forecast-%s.csv", today()))

# show a small portion of the resulting scores by atm
scoreByAtm <- subset(scoreByAtm, select = c(atm,trandate,usage,usageHat,mape,score))
head(scoreByAtm)
loginfo("...forecasting complete")

# TEMP DELETE ME
scoreByAtm <- subset(scoreByAtm, 
                trandate>'2013-06-30' & trandate<'2013-09-01', 
                select=c(atm,trandate,usage,usageHat,mape,score))
scoreByAtm[, sum(score), by=month(trandate)]







