#!/usr/bin/env Rscript
#
# a monte carlo simulation that calculates the fault risk for an ATM on a given schedule. 
#
library ("lubridate")
source ("simulation.R")

basicConfig (level = loglevels ["INFO"])
set.seed(123123)

#
# TODO - PERFORMANCE
#
# 2. merge forecast
# 3. daily min balance calculation
#

#
# estimates out-of-cash risk based on a monte carlo simulation
#
ooc.risk <- function (iters, schedules, atms, dates, capacities, forecast) {
    
    schedules.count <- length(unique(schedules$schedule))
    
    # initialize the data table that will be used for the simulation.  there is no
    # giant set of nested for loops.  there is a single data table containing all
    # of the necessary calculations for the simulation
    sim <- CJ (atm = atms, schedule = 1:schedules.count, iter = 1:iters, date = dates)
    
    # add day-of-week
    dow <- data.table(date = dates, 
                      dow  = substr (wday (dates, label = T), 1, 3), 
                      key  = "date")
    setkey (sim, date)
    sim [ dow, dow := dow]
    
    # add the bin capacity - TODO use the bin capacity analysis
    setkeyv(sim, c("atm"))
    sim [capacities, cash.max := cash.max]
    sim [, cash.min := 0 ]
    
    # when does service occur?
    setkeyv(sim, c("schedule", "dow"))
    sim[ schedules, service := service ]
    
    # add the forecast data 
    setkeyv(sim, c("atm","date"))
    sim [forecast, demand := demand]
    
    # add the vendor arrival data 
    # TODO - need to get real data
    forecast.size <- length(atms) * length(dates)
    sim[service == 1, demand.after := round((1 - runif(forecast.size, min=0.25, max=0.75)) * demand)]
    
    # the vendor always brings enough cash to fill the bin 
    sim[, supply := service * cash.max]
    
    # calculate the daily ending balance
    sim [, balance.end := cumsum.bounded(demand + supply, 
                                         start = 70000,   # TODO NEEDS TO COME FROM ATM
                                         lower = cash.min, 
                                         upper = cash.max), 
         by = list (iter, schedule, atm) ]
    
    # calculate the daily minimum balance
    sim [service == 1, balance.min := balance.end - (demand.after + supply)]
    
    # mark the faults
    sim [, fault := FALSE]
    sim [balance.min < 0, fault := TRUE]
    
    #
    # TODO - calculate overfill and overdraw
    # 
    
    # calculate the fault risk for each (atm, schedule).  this is effectively
    # the mean of the fault risk over each iteration.
    risks <- sim [, list (
        ooc.risk = sum(fault) / .N,
        iters = max(iter)
    ), by = list(atm, schedule)]    
}


main <- function(atm.count = 10, iterations = 500, days = 120) {
    
    # calculate the out-of-cash risk
    atms <- fetch.atms()[1:atm.count]
    dates <- fetch.dates()
    risks <- ooc.risk (iterations, 
                       fetch.schedules(), 
                       atms, 
                       dates, 
                       fetch.capacities(atms), 
                       fetch.forecast(atms, dates)) 
    
    # export the results
    export.file = "fault-risks.csv"
    write.csv (risks, export.file, row.names = FALSE)
    loginfo ("risks exported to '%s'", export.file)    
    
}




