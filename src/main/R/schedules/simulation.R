library ("logging")
library ("foreach")
library ("plyr")
library ("reshape2")
library ("data.table")
library ("Rcpp")

sourceCpp ("balances.cpp")

#
# fetch the set of ATMs
#
fetch.atms <- function () {
    profiles <- readRDS ("../../resources/profiles.rds")
    profiles <- data.table (profiles, key = "atm")
    atms <- (unique(as.character(profiles$atm)))
    
    return (atms)
}

#
# fetch the ATM cash bin capacities.  these are treated
# as fixed values.
#
fetch.capacities <- function() {
    
    # fetch the atm profiles
    profiles <- readRDS ("../../resources/profiles.rds")
    profiles <- data.table (profiles, key = "atm")
    
    # pull out the cash bin capacities
    capacities <- profiles [, list(atm, atmcapacity)]
    setnames (capacities, "atmcapacity", "cash.max")
    setkeyv (capacities, c("atm"))
    
    return (capacities)
}

#
# defines all of the valid schedules
#
fetch.schedules <- function() {
    
    # TODO missing bi-weekly and monthly schedules
    
    schedules <- CJ (
        "Sun"    = 0:1,  
        "Mon"    = 0:1, 
        "Tue"    = 0:1, 
        "Wed"    = 0:1, 
        "Thu"    = 0:1, 
        "Fri"    = 0:1, 
        "Sat"    = 0:1  )
    schedules[, schedule := 1:nrow(schedules) ]
    setcolorder(schedules, c(8, 1:7))
    setkey(schedules, "schedule")
    
    # reshape schedules from wide to long so that it is easier to merge with later
    schedules <- data.table( melt(schedules, id = c("schedule")))
    setnames(schedules, c("variable","value"), c("day.of.week","service"))
    setkeyv(schedules, c("schedule","day.of.week"))
    
    return (schedules)
}

#
# define the period of which the simulation will occur
# 
fetch.dates <- function (start = as.Date("2013-09-01"), days) {
    seq(start, by = 1, length.out = days)
}

#
# fetch the forecast
#
fetch.forecast <- function (atms, dates, iters) {
    
    # a forecast value is needed for each (atm, date, iter)
    forecast <- CJ (atm = atms, date = dates, iter = 1:iters)
    setkeyv (forecast, c("atm","date","iter"))
    
    # fetch the withdrawal forecast mean and bounds
    forecast.params <- readRDS("../../resources/forecast-withdrawals.rds")
    setnames (forecast.params, "trandate", "date")
    
    # TODO - is my std deviation calculation correct?
    # choose a normally distributed random value for each iteration
    forecast [forecast.params, 
              demand := round (rnorm (n    = iters, 
                                      mean = cash.hat, 
                                      sd   = (2 * (upper.bound.95 - cash.hat)) / 3.92))]

    # the demand will be NA, if there is no forecast for the given date
    return (forecast [!is.na(demand)])
}


#
# cumulative mean
#
cum.mean <- function(x) cumsum(x) / seq_along(x)

