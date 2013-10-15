#!/usr/bin/env Rscript

# required libraries
library("plyr", quietly=T)
library("caret", quietly=T)
library("data.table", quietly=T)
library("lubridate", quietly=T)
library("logging", quietly=T)
library("foreach", quietly=T)

# other project sources
source("../common/cache.R")
source("../common/utils.R")
source("../common/fetch.R")

basicConfig(level=loglevels["INFO"])

# fetch and clean the input data
withd <- cache("withd", {
    fetch( history.file = "deposits-all.rds",
           forecast.to  = today() + 30,
           data.dir     = "../../resources")
})

# train and score the model by atm
score.by.atm <- cache("deposit-score-by-atm", {
    withd[ 
        eval(subset.expr), 
        c("usage.hat","ape","score") := trainAndScore(
            .BY, .SD, 
            method = "gbm",
            split.at = as.Date(opts$splitAt), 
            default = expand.grid(.interaction.depth=2, .n.trees=50, .shrinkage=0.1), 
            verbose = F, 
            distribution = "poisson"), 
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



