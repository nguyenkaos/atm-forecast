#!/usr/bin/env Rscript

# defines the options/arguments
library("optparse")
all_options <- list(
    make_option(c("-p", "--parallel"), 
                action="store_true",
                help="Run with a parallel backend enabled",
                default=F)
)
opts <- parse_args(OptionParser(option_list=all_options))

library("logging")
library("plyr")
source("seatgeek.R")    
source("events.R")
basicConfig(level="INFO")

#
# Drives the process of fetching events for all ATMs.
#
main <- function() {
    atms <- fetchATMs()
    saveRDS(atms, "atms-to-geo.rds")
    
    poi <- extractPOI(atms)
    events <- fetchEvents(poi)
    saveRDS(events, "geo-to-events.rds")
}

#
# should the process by run in parallel?
#
if(!opts$parallel) {
    main()
    
} else {
    loginfo("execution proceeding in parallel")
    
    # execute in parallel
    library("doSNOW")
    registerDoSNOW(cl <- makeCluster(4, type="SOCK", outfile=""))
    
    # initialize each of the slaves
    clusterEvalQ(cl, {
        library("logging")
        library("plyr")
        source("seatgeek.R")    
        source("events.R")
        basicConfig(level="INFO")
    })
    
    # do stuff, then stop the cluster
    tryCatch({
        main()
        
    }, finally = {
        stopCluster(cl)
    })
}
