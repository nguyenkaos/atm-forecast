
library ("data.table")
library ("caret")
library ("gdata")
library ("RJSONIO")
library ("plyr")

source ("maps.R")
source ("../forecast/fetch.R")

# grab the location of each ATM
profiles <- readRDS("../../resources/profiles.rds")
locations <- data.table (profiles [, c("atm","longitude","latitude")], key = "atm")
setnames (locations, "latitude", "lat")
setnames (locations, "longitude", "lon")

# calculate deposits by week for each ATM in 2013
deposits <- fetch ("deposits-macro.rds")
deposits.weekly <- deposits [ 
    trandate >= "2013-01-01" & trandate < "2013-10-01", 
    list (depo = sum (usage, na.rm = T)), 
    by = list (atm, week (trandate))]
setkeyv (deposits.weekly, c("atm", "week"))

# center and scale the weekly deposits
d <- deposits.weekly[, list(depo)]
pp <- preProcess (d, method = c("scale"))
deposits.weekly$depo.norm <- predict (pp, newdata = d)

# calculate withdrawals by week for each ATM in 2013
withdrawals <- fetch ("withdrawals-macro.rds")
withd.weekly <- withdrawals [
    trandate >= "2013-01-01" & trandate < "2013-10-01", 
    list (withd = sum (usage, na.rm = T)), 
    by = list (atm, week (trandate))]
setkeyv (withd.weekly, c("atm", "week"))

# center and scale the weekly withdrawals
w <- withd.weekly[, list(withd)]
pp <- preProcess (w, method = c("center","scale"))
withd.weekly$withd.norm <- predict (pp, newdata = w)

# merge the deposits and withdrawals
usage.weekly <- merge (withd.weekly, deposits.weekly, by = c("atm","week"), all = T)
usage.weekly [is.na(withd), `:=`(withd = 0, withd.norm = 0.0) ]
usage.weekly [is.na(depo), `:=`(depo = 0, depo.norm = 0.0) ]

usage.weekly [, usage.norm := sum (withd.norm, depo.norm, na.rm = T), 
              by = list(atm, week) ]

# add the faults data
faults <- readRDS("../../resources/faults.rds")
faults.weekly <- faults [
    ,list (faults = length(start.time)), 
    by = list(atm, week(start.time)) ]
setkeyv (faults.weekly, c("atm","week"))

# merge the faults data with usage data
usage.weekly [faults.weekly, faults := faults]
usage.weekly [is.na (faults), faults := 0.0]

# calculate faults per usage
usage.weekly [usage.norm > 0, 
              fault.rate := faults / usage.norm, 
              by = list (atm, week)]
usage.weekly [is.na (fault.rate), fault.rate := 0]

# summarize by ATM
usage <- usage.weekly [, list(
    withd      = sum(withd),
    depo       = sum(depo),
    faults     = sum(faults),
    fault.rate = sum(fault.rate)
), by = atm]

# merge the location data with the usage and fault data
keep(usage, locations, sure = T)
usage [locations, `:=`(lon = lon, lat = lat)]
keep (usage, sure = T)

# export to json
usage.out <- unname (alply (usage, 1, identity))
out <- file("atms.json")
json <- toJSON(usage.out)
cat(json, file = out)
close(out)


# map it!
map.faults ("Chicago, IL", plot, zoom = 11)
map.faults ("Columbus, OH", plot, zoom = 9)



