library("data.table")
library("plyr")

source("seatgeek.R")

#
# Grab a set of ATMs that have sufficient data to work with.
#
fetchATMs <- function() {
    
    # get all ATMs that have sufficient usage history
    usage <- data.table(readRDS("../../resources//usage-all.rds"), key="atm")
    usage <- usage[, list(history=length(usage)), by=atm]
    usage <- usage[history>30,,]
    
    # get all ATMs that have location data
    profiles <- data.table(readRDS("../../resources/profiles.rds"), by="atm")
    profiles <- subset(profiles, select=c(atm, latitude, longitude)) 
    profiles$latitude[profiles$latitude==0] <- NA
    profiles$longitude[profiles$longitude==0] <- NA
    profiles <- subset(profiles, !is.na(longitude))
    
    # final set of ATMs that have sufficient history and location data
    profiles <- merge(profiles, usage, by="atm")
    
    # assign each a 'geo.id' based on lat/long
    profiles$geo.id <- id(profiles[,c("longitude","latitude"), with=F], drop=T)
    return(profiles)
}

#
# Extract a set of unique 'points of interest'
#
extractPOI <- function(atms) {
    atms <- subset(atms, select=c(geo.id, longitude, latitude))
    poi <- atms[!duplicated(atms)]
    setkey(poi, geo.id)
    return(poi)
}

#
# Fetches events 'close' to each of the ATMs in the fleet.
#
fetchEvents <- function(poi, api=SeatGeekR$new(), parallel=F) {
    
    # grab events near each 'point of interest'
    events <- ddply(poi, "geo.id", function(p) {
        loginfo("fetching events for geo.id '%s'", p$geo.id)
        events <- tryCatch(
            api$events(lat=p$latitude, lon=p$longitude, range="1mi"), 
            error=function(e) logerror("error for geo.id '%s': %s", p$geo.id, e))
    }, .parallel=parallel)
    
    # clean-up the resulting events data
    events <- rename(events, c("id"="event.id"))
}

logError <- function(e) {
    logerror("unexpected error: %s", e)
}
