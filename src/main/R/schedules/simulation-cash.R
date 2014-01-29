#!/usr/bin/env Rscript
#
# a monte carlo simulation that calculates the fault risk for an ATM on a given schedule. 
#
# TODO
# (1) The day 1 balance for the simulation is set to 0. What should this be?
# (2) Add in the bi-weekly and monthly schedules.
# (3) Performance - Will this scale?
#
library ("lubridate")
source ("simulation.R")

basicConfig (level = loglevels ["INFO"])
#set.seed(123123)

#
# estimates out-of-cash risk based on a monte carlo simulation
#
simulate.cash.risk <- function (iters, schedules, atms, dates, capacities, forecast) {
    
    schedules.count <- length (unique (schedules$schedule))
    
    # initialize the data table that will be used for the simulation.  there is no
    # giant set of nested for loops.  there is a single data table containing all
    # of the necessary calculations for the simulation
    sim <- CJ (date     = dates,
               atm      = atms, 
               schedule = 1:schedules.count, 
               iter     = 1:iters)
    
    # add day-of-week
    dow <- data.table(date = dates, 
                      dow  = substr (lubridate::wday (dates, label = T), 1, 3), 
                      key  = "date")
    setkey (sim, date)
    sim [ dow, dow := dow]
    
    # when does service occur?
    setkeyv (sim, c("schedule", "dow"))
    sim[ schedules, service := service ]
    
    # add the forecast data 
    setkeyv (sim, c("atm","date","iter"))
    sim [forecast, demand := -demand]   
    
    # add the bin capacity - TODO use the bin capacity analysis
    sim [capacities, cash.max := cash.max]
    sim [, cash.min := 0 ]
    
    # add the vendor arrival data 
    forecast.size <- length(atms) * length(dates)
    sim[service == 1, demand.split := runif(forecast.size, min=0.25, max=0.75)]
    sim[service == 0, demand.split := 1]
    
    # distribute demand based on when the vendor arrives
    sim [, demand.early := round (demand * demand.split)]
    sim [, demand.late := demand - demand.early]
    
    # the vendor always brings enough cash to fill the bin 
    sim[, supply := service * cash.max ]
    
    # calculate the daily ending balance
    sim [, 
         c("balance","fault","demand.excess","supply.excess") := 
             balances (0, demand.early, supply, demand.late, cash.max),
         by = list (iter, schedule, atm) ]
    
    # summarize the simulation results
    risks <- sim [, list(
        fault.risk    = sum (fault) / .N,
        demand.excess = sum (demand.excess) / .N,
        supply.excess = sum (supply.excess) / .N
    ), by = list(atm, schedule)]    
}

fault.risks <- function(start.date   = '2013-11-10',
                        atm.count    = 1, 
                        iters        = 20, 
                        days         = 120, 
                        delta.max    = 0.01, 
                        attempts.max = 20, 
                        attempts.min = 3) {
    
    # data required for the simulation
    atms       <- fetch.atms() [1:atm.count]
    dates      <- fetch.dates(start = as.Date(start.date), days = days)
    schedules  <- fetch.schedules()
    capacities <- fetch.capacities()
    
    # simulate each ATM separately
    for(atm in atms) {
        
        # will track the calculated risk after each simulation run
        risks <- data.table ()
        for(i in 1:attempts.max) {
            loginfo("[%s] executing %d iterations of %d possible", atm, i * iters, attempts.max * iters)
            
            # draw new samples independently for each iteration
            forecast <- fetch.forecast(atms, dates, iters)
            
            # run a simulation to calculate the risk
            risks <- rbindlist (list (
                risks, 
                simulate.cash.risk (iters, schedules, atms, dates, capacities, forecast)))        
            
            # once the fault risk converges, no further simulations needed
            summary  <- risks [, list (
                fault.risk    = mean (fault.risk),
                delta         = diff (range (tail (cum.mean (fault.risk), attempts.min))),
                attempts      = .N
            ), by = list (atm, schedule)]
            
            if (max (summary$attempts) > attempts.min && max (summary$delta) < delta.max)
                break;
        }
    }
    
    # the final result is the mean of all simulations
    results <- risks [, list (
        fault.risk    = mean (fault.risk),
        demand.excess = mean (demand.excess),
        supply.excess = mean (supply.excess),
        iters         = .N * iters
    ), by = list (atm, schedule)]
    
    # export the results
    export.file = "cash-simulation-results.csv"
    write.csv (results, export.file, row.names = FALSE)
    loginfo ("risks exported to '%s'", export.file)    
    
    return (results)
}


