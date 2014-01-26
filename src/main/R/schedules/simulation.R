library ("logging")
library ("foreach")
library ("plyr")
library ("reshape2")
library ("data.table")

source ("cumsum-bounded.R")

#
# fetch the set of ATMs
#
fetch.atms <- function () {
    profiles <- readRDS ("../../resources/profiles.rds")
    profiles <- data.table (profiles, key = "atm")
    atms <- (unique(as.character(profiles$atm)))
    atms <- as.factor (atms)
    
    return (atms)
}

#
# fetch the ATM bin capacities
#
fetch.capacities <- function(atms, iters) {
    
    # TODO - need to get real bin capacities
    capacity.size <- length(atms) * iters
    capacity <- data.table (atm      = atms, 
                            iter     = 1:iters,
                            cash.max = round (rnorm (capacity.size, mean = 70000, sd = 20000)))
    setkeyv (capacity, "atm")
    
    return (capacity)
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
    
    result <- CJ (atm = atms, date = dates, iter = 1:iters)
    setkeyv (result, c("atm","date"))

    # fetch the actual forecast
    forecast <- readRDS("../../resources/forecast-withdrawals.rds")
    setnames (forecast, "trandate", "date")
    
    # TODO - DONT THINK THIS IS WORKING - WHY IS ITER = NA IN SOME CASES
    foo <- result [forecast, allow.cartesian = TRUE]
    
    
    
    # calculate the std deviation with a 99% confidence interval; ignore lower bound
    result [, sd := sqrt(1) * ((2 * (upper.bound.99 - cash.hat)) / 5.15 )]
    
    # create a forecasted value from a normal distribution
    result [, demand := rnorm(1, mean = cash.hat, sd = sd), by = list(atm, trandate) ]
    
    # TODO need to get the real forecast!
    #forecast.size <- length(atms) * length(dates) * iters
    
    return (result)
}