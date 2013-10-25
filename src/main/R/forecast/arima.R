#!/usr/bin/env Rscript

library("data.table")
library("lubridate")
library("forecast")

source("../forecast/fetch.R")

# split for test versus training sets
split.at <- as.Date("2013-09-01")

# grab the deposits history
path <- "../../resources/deposits-mini.rds"
deposits <- cache( basename.only (path), {
    fetchHistory (path, today())
})
setkeyv (deposits, c("atm", "trandate"))

# tranform the 'long' history to a 'wide' time series
train <- deposits [
    atm == "AZ2011" & trandate <= split.at & !is.na(usage),
    list (
        usage = list (usage),
        min   = min(trandate),
        max   = max(trandate)
    ), by = atm ]

test <- deposits [
    atm == "AZ2011" & trandate > split.at & !is.na(usage),
    list (
        usage = list (usage),
        min   = min(trandate),
        max   = max(trandate)
    ), by = atm ]

# fit an arima model
train.usage <- unlist( train [["usage"]])
fit <- Arima (train.usage, order = c(0, 2, 1))

# predict using the fit
test.usage <- unlist( test [["usage"]])
usage.hat <- c(fitted (fit), 
               predict (fit, n.ahead = length(test.usage), se.fit = FALSE ))

# convert back to a "long" history from "wide" time series
deposits <- data.table( atm       = c("AZ2011"), 
                        trandate  = seq(train$min, test$max, by = "day"),
                        usage     = c(train.usage, test.usage),
                        usage.hat = usage.hat )

# plot that shit
zoom <- deposits [trandate > test$max - 90]
plot(usage ~ trandate, zoom, type = "b", col = "blue")
lines(usage.hat ~ trandate, zoom, type = "b", col = "red")





