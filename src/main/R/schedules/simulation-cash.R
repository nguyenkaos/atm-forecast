#
# a monte carlo simulation that calculates the fault risk for an ATM on a given schedule. 
#

library ("logging")
library ("foreach")
library ("plyr")
library ("reshape2")

basicConfig (level = loglevels ["INFO"])

set.seed(123123)

#
# the set of ATMs
#
profiles <- readRDS ("../../resources/profiles.rds")
profiles <- data.table (profiles, key = "atm")
atms <- (unique(as.character(profiles$atm)))[1]

#
# obtain the ATM cash/bin capacity - TODO get real capacities
#
capacity <- data.table (atm = atms, cash.capacity = rep(160000, length(atms)))
setkeyv (capacity, "atm")

#
# define all of the possible schedules
#
# TODO missing bi-weekly and monthly schedules
# TODO update with 
#
schedules <- CJ (
    "Sun"    = 0:1,  
    "Mon"    = 0:1, 
    "Tue"    = 0:1, 
    "Wed"    = 0:1, 
    "Thu"    = 0:1, 
    "Fri"    = 0:1, 
    "Sat"    = 0:1  )
schedules[, schedule.id := 1:nrow(schedules) ]
setcolorder(schedules, c(8, 1:7))
setkey(schedules, "schedule.id")

#
# reshape schedules from wide to long so that it is easier to merge with later
#
schedules <- data.table( melt(schedules, id = c("schedule.id")))
setnames(schedules, c("variable","value"), c("day.of.week","service"))
setkeyv(schedules, c("schedule.id","day.of.week"))
schedules.count <- length(unique(schedules$schedule.id))

#
# define the period of which the simulation will occur
# 
dates.start <- as.Date("2013-09-01")
dates.end <- as.Date("2013-12-31") 
dates <- seq(dates.start, dates.end, by = 1)

#
# define the simulation data that has atms, date, and schedules
#
sim <- CJ (schedule.id = 1:schedules.count, atm = atms, date = dates )
sim [, day.of.week := lubridate::wday (date, label = TRUE, abbr = TRUE) ]
sim [, day.of.week := substr(day.of.week, 1, 3) ]

#
# add the bin capacity - TODO use the bin capacity analysis
#
setkeyv(sim, c("atm"))
sim [capacity, cash.capacity := cash.capacity]

#
# does service occur?
#
setkeyv(sim, c("schedule.id", "day.of.week"))
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
sim[, demand.before.percent := runif(forecast.size, min=0.25, max=0.75)]
sim[, demand.before := round(demand * demand.before.percent) ]
sim[, demand.after := demand - demand.before]

#
# the vendor always brings enough cash to fill the bin 
#
sim[, supply := service * cash.capacity]

#
# calculate the projected minimum balance
#
sim[, delta := demand + supply]
sim[ date == dates.start, delta := cash.capacity, ]
sim[, balance.end.projected := cumsum(delta), by = list(schedule.id, atm)]

#
# when the projected min balance goes below 0, need to adjust the delta as this is not possible
# TODO - stuck here
#
sim[ balance.end.projected < 0, delta.adjusted := -(balance.end.projected - delta)]
sim[ balance.end.projected < 0, delta.adjusted := min(0, delta.adjusted)]

#
# calculate the projected minimum balance.  pretend the bin is full
# on day 1.  this may need to change.
#
# TODO - this is wrong - calc end balance first??
#
#sim[ date == dates.start, demand.before := cash.capacity, ]
#sim[, balance.min.projected := cumsum(demand.before), by = list(schedule.id, atm)]

setkeyv(sim, c("atm", "schedule.id"))


