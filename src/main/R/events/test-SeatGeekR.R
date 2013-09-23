

# even with stats hard to tell magnitude of the event
# a performer can contibute to magnitude
# a venue can contribute to the magnitude
    
source("SeatGeekR.R")
geek <- SeatGeekR$new()

# ohio stadium
#result <- geek$venues(lat=40.0032, lon=-83.0195, range="1mi")
result <- geek$events(lat=40.0032, lon=-83.0195, range="1mi", datetime_utc.gt="2012-09-07")

# attempt 2
flat <- do.call(c, unlist(result$events, recursive=F))
eventFields <- c(1, 6, 9, 324) # event id, title, type, datetime_utc
statsFields <- grep("^stats.*", names(flat))
venueFields <- grep("^venue.*", names(flat))
keepers <- sapply(flat, "[", c(eventFields, statsFields, venueFields))
df <- data.frame(matrix(keepers, nrow=length(result$events), byrow=T))

# attempt 1 - this does work but I have to manually specify all the fields
events <- result$events
events <- sapply(events, "[", c("id","title","type","datetime_utc","venue.id","venue.name"))
df <- data.frame(matrix(events, nrow=result$meta$per_page, byrow=T))
colnames(df) <- row.names(events)

