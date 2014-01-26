#!/usr/bin/env Rscript
#
# a monte carlo simulation that calculates the fault risk for an ATM on a given schedule. 
#
library ("lubridate")
source ("simulation.R")

basicConfig (level = loglevels ["INFO"])
#set.seed(123123)

#
# estimates out-of-cash risk based on a monte carlo simulation
#
ooc.risk <- function (iters, schedules, atms, dates, capacities, forecast) {
    
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
    
    # add the forecast data 
    setkeyv(sim, c("atm","date"))
    sim [forecast, demand := demand]    
    
    # add the bin capacity - TODO use the bin capacity analysis
    #setkeyv(sim, c("atm"))
    sim [capacities, cash.max := cash.max]
    sim [, cash.min := 0 ]
    
    # when does service occur?
    setkeyv(sim, c("schedule", "dow"))
    sim[ schedules, service := service ]
    
    # add the vendor arrival data 
    # TODO - need to get real data
    forecast.size <- length(atms) * length(dates)
    sim[service == 1, demand.after := round((1 - runif(forecast.size, min=0.25, max=0.75)) * demand)]
    sim[service == 0, demand.after := 0]
    
    # the vendor always brings enough cash to fill the bin 
    sim[, supply := service * cash.max]
    
    # calculate the daily ending balance
    sim [, balance.end := cumsum.bounded(demand + supply, 
                                         start = 70000,   # TODO NEEDS TO COME FROM ATM
                                         lower = cash.min, 
                                         upper = cash.max), 
         by = list (iter, schedule, atm) ]
    
    # calculate the daily minimum balance
    sim [, balance.min := balance.end - (demand.after + supply)]
    
    # mark the faults
    sim [, fault := FALSE]
    sim [balance.min <= 0, fault := TRUE]
    
    #
    # TODO - calculate overfill and overdraw
    # 
    
    # calculate the fault risk for each (atm, schedule).  effectively
    # the mean of the fault risk over each iteration.
    risks <- sim [, sum(fault) / .N, by = list(atm, schedule)]    
}

cum.mean <- function(x) cumsum(x) / seq_along(x)

main <- function(atm.count = 10, iters = 50, days = 120, delta.max = 0.01, attempts.max = 20, attempts.min = 3) {
    
    # data required for the simulation
    atms <- fetch.atms()[1:atm.count]
    dates <- fetch.dates(days = days)
    schedules <- fetch.schedules()
        
    for(atm in atms) {
  
        # will track the calculated risk after each simulation run
        risks <- data.table (atm = atm, 
                             schedule = unique (schedules$schedule), 
                             fault.risk = c(Inf),
                             delta = c(Inf),
                             key = c("atm","schedule"))
        for(i in 1:attempts.max) {
            
            # draw new samples # TODO these need to be different on each iteration
            capacities <- fetch.capacities(atms, iters)
            forecast <- fetch.forecast(atms, dates, iters)
            
            # run a simulation to calculate the risk
            risks.last <- ooc.risk (iters, schedules, atms, dates, capacities, forecast) 
            
            # merge the latest risk calculation with the rest
            col.name <- quote(paste("iter", as.character(i * iters), sep = "."))
            risks [ risks.last, eval(col.name) := V1]        
            
            # do we need to continue?
            risks [, fault.risk := mean (unlist (.SD)),  by = list(atm, schedule, fault.risk, delta)]
            risks [, delta := max (diff (range (tail (cum.mean (unlist (.SD)), 3)))), by = list(atm, schedule, fault.risk, delta)]
            if(i > attempts.min && max (risks$delta) < delta.max )
                break;
            
            risks[]
        }

    }
    
    # export the results
    export.file = "fault-risks.csv"
    write.csv (risks [, list(atm, schedule, fault.risk)], export.file, row.names = FALSE)
    loginfo ("risks exported to '%s'", export.file)    
    
    return (risks)
}




