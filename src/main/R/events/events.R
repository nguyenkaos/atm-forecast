#!/usr/bin/env Rscript

library("doSNOW")
registerDoSNOW(cl <-makeCluster(2, type="SOCK", outfile=""))

clusterEvalQ(cl, {
    library("logging")
    library("plyr")
    source("../common/cache.R")
    source("SeatGeekR.R")    
    basicConfig(level="INFO")
})

fetchEvents <- function() {
    
    # fetch the profiles for each ATM
    profiles <- readRDS("../../resources/profiles.rds")
    profiles <- subset(profiles, select=c(terminal.id, latitude, longitude))
    profiles <- profiles[1:10,]
    
    # grab events near any of the ATMs
    geek <- SeatGeekR$new()
    events <- ddply(profiles, "terminal.id", function(p) {
        loginfo("fetching events for atm %s", p$terminal.id)
        
        events <- NULL
        if(!is.na(p$latitude) && !is.na(p$longitude)) {
            events <- geek$events(lat=p$latitude, lon=p$longitude, range="1mi")
        }
        
        return(events)
    }, .parallel=TRUE)
    
    # clean-up the resulting events data
    events <- rename(events, c("id"="event.id"))
}

events <- cache("seatgeek-events", fetchEvents())

#events <- fetchEvents()
#saveRDS(events, "seatgeek-events.rds")

stopCluster(cl)


