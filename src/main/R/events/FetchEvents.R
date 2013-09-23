
library("data.table")
library("plyr")

source("SeatGeekR.R")

fetchEvents <- function() {
    
    # find the location of each ATM
    geo <- readRDS("../../resources/profiles.rds")
    geo <- subset(geo, select=c("terminal_id","LATITUDE","LONGITUDE"))
    names(geo) <- c("atm","lat","lon")
    
    # find the start/end of the usage data for each ATM
    usage <- readRDS("../../resources/usage-all.rds")
    usage <- data.table(usage)
    usage <- usage[,list(min=min(trandate), max=max(trandate)), by=atm]
    
    # merge the data sets
    atms <- merge(usage, geo, by="atm")
    
    # find all events close to each atm
    atms <- atms[1:100,]
    
    geek <- SeatGeekR$new()
    events <- ddply(atms, atm, function(atm) {
        geek$events(lat=atm$lat, lon=atm$lon, range="1mi", datetime_utc.gte=atm$min)
    })    
}