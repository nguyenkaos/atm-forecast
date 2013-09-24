#!/usr/bin/env Rscript

library("logging")
library("plyr")

source("SeatGeekR.R")
source("../common/cache.R")

basicConfig(level="INFO")

fetchEvents <- function() {
    
    # fetch the profiles for each ATM
    profiles <- readRDS("../../resources/profiles.rds")
    profiles <- subset(profiles, select=c(terminal.id, latitude, longitude))
    #profiles <- profiles[1:10,]
    
    # grab events near any of the ATMs
    geek <- SeatGeekR$new()
    events <- ddply(profiles, "terminal.id", function(p) {
        loginfo("fetching events for atm %s", p$terminal.id)
        
        events <- NULL
        if(is.na(p$latitude) || is.na(p$longitude)) {
            loginfo("missing location for ATM %s", p$terminal.id)
        } else {
            events <- geek$events(lat=p$latitude, lon=p$longitude, range="1mi")
        }
        
        return(events)
    })
    
    events <- rename(events, c("id"="event.id"))
}

events <- cache("events", fetchEvents())

