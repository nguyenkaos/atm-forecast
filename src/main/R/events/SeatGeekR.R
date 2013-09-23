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
        fromJSON(getForm(paste(self$baseURL, "events", sep="/"), 
                         .opts=curlOptions(verbose=verbose),
                         ...))        
    }
    
    # search for performers
    performers <- function(self, ..., verbose=F) {
        fromJSON(getForm(paste(self$baseURL, "performers", sep="/"), 
                         .opts=curlOptions(verbose=verbose),
                         ...))
    }
    
    # search for venues
    venues <- function(self, ..., verbose=F) {
        fromJSON(getForm(paste(self$baseURL, "venues", sep="/"), 
                         .opts=curlOptions(verbose=verbose),
                         ...))
    }
    
    # search for taxonomies
    taxonomies <- function(self, ..., verbose=F) {
        fromJSON(getForm(paste(self$baseURL, "taxonomies", sep="/"), 
                         .opts=curlOptions(verbose=verbose),
                         ...))
    }
})