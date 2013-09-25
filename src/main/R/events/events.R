library("data.table")
library("dplyr")

source("seatgeek.R")


#
# TODO - remove duplicate locations and store another geo.id -> events
# table
#
profiles <- readRDS("../../resources/profiles.rds")
profiles <- subset(profiles, select=c(atm, latitude, longitude)) 

# remove those with invalid geo data
profiles$latitude[profiles$latitude==0] <- NA
profiles$longitude[profiles$longitude==0] <- NA
profiles <- subset(profiles, !is.na(longitude))

# assign each atm to a group based on geo
# TODO - this is not returning unique groups
profiles$geo.id <- id(profiles[c("longitude","latitude")], drop=T)

# analyze the groups - how much time are we saving here?
profiles <- data.table(profiles)
geos <- profiles[, list(latitude, longitude, count=length(atm)), by=geo.id]
geos <- geos[order(-count)]

#
# Fetches events 'close' to each of the ATMs in the fleet.
#
fetchEvents <- function(parallel=F) {
    
    # fetch the profiles for each ATM
    profiles <- readRDS("../../resources/profiles.rds")
    profiles <- subset(profiles, select=c(atm, latitude, longitude)) 
    profiles <- data.table(profiles)
    
    # grab events near any of the ATMs
    geek <- SeatGeekR$new()
    fetch <- function(p) {
        loginfo("fetching events for atm %s", p$atm)
        
        # lat/lon required
        events <- NULL
        if(!is.na(p$latitude) && !is.na(p$longitude)) {
            events <- tryCatch(
                geek$events(lat=p$latitude, lon=p$longitude, range="1mi"), 
                error=function(e) logerror("error for %s: %s", p$atm, e))
        }
    }
    events <- profiles[, fetch(.SD), by=atm]
        
    # clean-up the resulting events data
    events <- rename(events, c("id"="event.id"))
}

logError <- function(e) {
    logerror("unexpected error: %s", e)
}
