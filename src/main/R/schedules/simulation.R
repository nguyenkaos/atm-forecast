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
fetch.dates <- function (start = as.Date("2013-09-01"), days = 120) {
    seq(start, by = 1, length.out = days)
}

#
# fetch the forecast
#
fetch.forecast <- function (atms, dates, iters) {
    
    # TODO need to get the real forecast!
    
    forecast.size <- length(atms) * length(dates) * iters
    forecast <- CJ (atm = atms, date = dates, iter = 1:iters)
    forecast [, demand := -round(runif(forecast.size, min=0, max=20000))]
    setkeyv(forecast, c("atm","date"))
    
    return (forecast)
}