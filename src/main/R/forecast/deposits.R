#!/usr/bin/env Rscript

# gather the command line options
source("options.R")
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

basicConfig(level=loglevels[opts$logLevel])

# fetch and clean the input data
deposits <- cache("deposits-features", {
    fetch( history.file = "deposits-all.rds",
           forecast.to  = today() + 30,
           data.dir     = "../../resources")
})

# train and score the model by atm
score.by.atm <- cache("deposit-score-by-atm", {
    deposits[ 
        eval(subset.expr), 
        c("usage.hat","ape","score") := trainAndScore(
            .BY, 
            .SD, 
            method       = "gbm",
            split.at     = as.Date(opts$splitAt), 
            default      = expand.grid(.interaction.depth=2, .n.trees=50, .shrinkage=0.1), 
            verbose      = F, 
            distribution = "poisson",
            cache.prefix = "deposits-fit"), 
        by=atm]
})

# extract the forecast
forecast <- score.by.atm [
    trandate >= today(), 
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
score.by.atm [
    trandate>'2013-06-30' & trandate<'2013-09-01',
    list(
        score=sum(score, na.rm=T),
        mape=mean(ape,na.rm=T),
        count=length(unique(atm))
    ), by=month(trandate)]
