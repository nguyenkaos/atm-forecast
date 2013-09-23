

library("plyr")

# even with stats hard to tell magnitude of the event
# a performer can contibute to magnitude
# a venue can contribute to the magnitude
    
source("SeatGeekR.R")
geek <- SeatGeekR$new()
result <- geek$events(lat=40.0032, lon=-83.0195, range="1mi", datetime_utc.gt="2012-09-07") # ohio stadium
