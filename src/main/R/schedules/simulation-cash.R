#!/usr/bin/env Rscript
#
# a monte carlo simulation that calculates the fault risk for an ATM on a given schedule. 
#

library ("logging")
library ("foreach")
library ("plyr")
library ("reshape2")
library ("data.table")

source ("cumsum-bounded.R")

basicConfig (level = loglevels ["INFO"])
set.seed(123123)

atm.count <- 90
iteration.count <- 50
days <- 120

#
# fetch the set of ATMs
#
profiles <- readRDS ("../../resources/profiles.rds")
profiles <- data.table (profiles, key = "atm")
atms <- (unique(as.character(profiles$atm)))[1:atm.count]
atms <- as.factor (atms)

#
# obtain the ATM cash/bin capacity - TODO get real capacities
#
cash.capacity <- 70000
capacity <- data.table (atm = atms, cash.max = rep(cash.capacity, length(atms)))
setkeyv (capacity, "atm")

#
# define all of the possible schedules
#
# TODO missing bi-weekly and monthly schedules
#
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

#
# reshape schedules from wide to long so that it is easier to merge with later
#
schedules <- data.table( melt(schedules, id = c("schedule")))
setnames(schedules, c("variable","value"), c("day.of.week","service"))
setkeyv(schedules, c("schedule","day.of.week"))
schedules.count <- length(unique(schedules$schedule))

#
# define the period of which the simulation will occur
# 
dates.start <- as.Date("2013-09-01")
dates <- seq(dates.start, by = 1, length.out = days)

#
# define the simulation data that has atms, date, and schedules
#
sim <- CJ (iteration = 1:iteration.count, schedule = 1:schedules.count, atm = atms, date = dates )
sim [, day.of.week := lubridate::wday (date, label = TRUE, abbr = TRUE) ]
sim [, day.of.week := substr(day.of.week, 1, 3) ]

#
# add the bin capacity - TODO use the bin capacity analysis
#
setkeyv(sim, c("atm"))
sim [capacity, cash.max := cash.max]
sim [, cash.min := 0 ]

#
# does service occur?
#
setkeyv(sim, c("schedule", "day.of.week"))
sim[ schedules, service := service ]

#
# add the forecast data - TODO use real forecast data
#
forecast.size <- length(atms) * length(dates)
forecast <- CJ (atm = atms, date = dates)
forecast [, demand := -round(runif(forecast.size, min=0, max=20000))]
setkeyv(forecast, c("atm","date"))
setkeyv(sim, c("atm","date"))
sim [forecast, demand := demand]

#
# add the vendor arrival data - TODO use real data
#
sim[service == 1, demand.after := round((1 - runif(forecast.size, min=0.25, max=0.75)) * demand)]

#
# the vendor always brings enough cash to fill the bin 
#
sim[, supply := service * cash.max]

#
# calculate the daily ending balance
#
sim [, balance.end := cumsum.bounded(demand + supply, 
                                     start = cash.capacity,   # TODO NEEDS TO COME FROM ATM
                                     lower = cash.min, 
                                     upper = cash.max), 
     by = list (iteration, schedule, atm) ]

#
# calculate the daily minimum balance
#
sim [service == 1, balance.min := balance.end - (demand.after + supply)]

#
# mark the faults
#
sim [, fault := FALSE]
sim [balance.min < 0, fault := TRUE]

#
# TODO - calculate overfill and overdraw
# 

#
# calculate the fault risk for each (atm, schedule).  this is effectively
# the mean of the fault risk over each iteration.
#
risks <- sim[, list (
    fault.risk = sum(fault) / .N,
    iterations = max(iteration)
), by = list(atm, schedule)]

#
# export the results
#
export.file = "fault-risks.csv"
write.csv (risks, export.file, row.names = FALSE)
loginfo ("risks exported to '%s'", export.file)


