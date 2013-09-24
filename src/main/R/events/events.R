
#
# Fetches events 'close' to each of the ATMs in the fleet.
#
fetchEvents <- function(parallel=F) {
    
    # fetch the profiles for each ATM
    profiles <- readRDS("../../resources/profiles.rds")
    profiles <- subset(profiles, select=c(terminal.id, latitude, longitude))
    
    # grab events near any of the ATMs
    geek <- SeatGeekR$new()
    events <- ddply(profiles, "terminal.id", function(p) {
        loginfo("fetching events for atm %s", p$terminal.id)
        
        # lat/lon required
        events <- NULL
        if(!is.na(p$latitude) && !is.na(p$longitude)) {
            events <- tryCatch(
                geek$events(lat=p$latitude, lon=p$longitude, range="1mi"), 
                error=function(e) logerror("error for %s: %s", p$terminal.id, e))
        }
    }, .parallel=parallel)
    
    # clean-up the resulting events data
    events <- rename(events, c("id"="event.id"))
}

logError <- function(e) {
    logerror("unexpected error: %s", e)
}