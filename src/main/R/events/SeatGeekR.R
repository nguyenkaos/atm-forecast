library(proto)
library(RCurl)
library(RJSONIO)

#
# http://platform.seatgeek.com/
#
SeatGeekR <- proto(expr={
    
    baseURL <- NULL
    
    # constructor
    new <- function(self, baseURL="http://api.seatgeek.com/2") {
        self$proto(baseURL=baseURL)
    }
    
    # search for events
    events <- function(self, ..., verbose=F) {
        self$fetch(what="events", ..., verbose=verbose)
    }
    
    # search for performers
    performers <- function(self, ..., verbose=F) {
        self$fetch("performers", ..., verbose)
    }
    
    # search for venues
    venues <- function(self, ..., verbose=F) {
        self$fetch("venues", ..., verbose)
    }
    
    # search for taxonomies
    taxonomies <- function(self, ..., verbose=F) {
        self$fetch("taxonomies", ..., verbose)
    }
    
    
    fetch <- function(self, what, ..., verbose=F) {
        json <- getForm(paste(self$baseURL, what, sep="/"), .opts=curlOptions(verbose=verbose), ...)
        asList <- fromJSON(json)
        to.data.frame(unique.names(asList[[2]], what))
    }    
})

##
# Ensure that each list element has a unique name.
#
unique.names <- function(aList, baseName, recursive=T) {
    # nothing to do; also ends recursion
    if(length(aList)==0)
        return(aList)
    
    # if the list elements are not named, name them
    if(is.null(names(aList))) {
        names(aList) <- paste(baseName, 1:length(aList), sep=".")
    }
    
    if(recursive) {
        # recursively rename any sub-lists
        attrNames <- attributes(aList)$names
        for(i in 1:length(attrNames)) {
            attr <- aList[[i]]
            if(is.list(attr)) {
                aList[[i]] <- unique.names(attr, attrNames[[i]])
            }
        }
    }
    
    return(aList)
}

##
# 
# 
to.data.frame <- function(aList) {
    as.data.frame(
        do.call(rbind.fill.matrix, lapply(aList, function(l) t(unlist(l))))
    )        
}

