
library ("data.table")
library ("caret")

source ("maps.R")
source ("../forecast/fetch.R")

# grab the location of each ATM
profiles <- readRDS("../../resources/profiles.rds")
locations <- data.table (profiles [, c("atm","longitude","latitude")], key = "atm")
setnames (locations, "latitude", "lat")
setnames (locations, "longitude", "lon")

# calculate deposits by week for each ATM in 2013
deposits <- fetch ("deposits-macro.rds")
weekly.deposits <- deposits [ 
    trandate >= "2013-01-01" , 
    list (depo = sum (usage, na.rm = T)), 
    by = list (atm, week (trandate))]
setkeyv (weekly.deposits, c("atm", "week"))

# calculate withdrawals by week for each ATM in 2013
withdrawals <- fetch ("withdrawals-all.rds")
weekly.withd <- withdrawals [
    trandate >= "2013-01-01" , 
    list (withd = sum (usage, na.rm = T)), 
    by = list (atm, week (trandate))]
setkeyv (weekly.withd, c("atm", "week"))

# load and clean the gasper data
gasper <- readRDS("../../resources/gasper.rds")
setkeyv(gasper, "atm")
faults <- gasper [, list (faults = length(ticket.key)), by = list(atm, week(start.time.dt))]
setkeyv (faults)

weekly <- weekly.deposits [weekly.withd]
weekly [gasper, faults := length(ticket.key)]





# center and scale the deposits data
d <- deposits[, list(deposits)]
pp <- preProcess (d, method = c("center","scale"))
deposits$deposits.norm <- predict (pp, newdata = d)

# find total deposits across each ATM
deposits <- deposits [trandate >= "2013-01-01", 
                      list (
                          deposits = sum (usage, na.rm = T),
                      ), by = atm]

# fetch the withdrawals history
withdrawals <- fetch ("withdrawals-all.rds")
withdrawals <- withdrawals [trandate >= "2013-01-01", list (withdrawals = sum (usage, na.rm = T)), by = atm]

input <- withdrawals[, list(deposits)]
pp <- preProcess (input, method = c("center","scale"))
deposits$deposits.norm <- predict (pp, newdata = input)

# merge the histories
usage <- deposits [withdrawals]
usage [, usage := sum(deposits, withdrawals, na.rm = T), by = atm]

# add the total usage to the profiles
profiles [usage, usage := usage]
profiles [is.na(usage), usage := 0 ]

# TODO FAULTS NEED CALCULATED BY WEEK

# add fault data to the profiles
faults <- gasper [, list(faults = length(ticket.key)), by = atm]
profiles [faults, fault.count := faults]
profiles [ is.na(fault.count), fault.count := 0L]

plot <- profiles [, list (
    atm,
    lat, 
    lon,
    usage,
    fault.count, 
    fault.per.usage = fault.count / (usage + 1)
)]

# map it!
map.faults ("Chicago, IL", plot, zoom = 11)
map.faults ("Columbus, OH", plot, zoom = 9)



