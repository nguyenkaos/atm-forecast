#!/usr/bin/env Rscript

library("doSNOW")
registerDoSNOW(cl <- makeCluster(4, type="SOCK", outfile=""))

library("logging")
library("plyr")
source("seatgeek.R")    
source("events.R")
basicConfig(level="INFO")

# initialize each of the slaves
clusterEvalQ(cl, {
    library("logging")
    library("plyr")
    source("seatgeek.R")    
    source("events.R")
    basicConfig(level="INFO")
})

tryCatch({
    events <- fetchEvents(parallel=T)
    saveRDS(events, "seatgeek-events.rds")
    
}, finally = {
    stopCluster(cl)
})







