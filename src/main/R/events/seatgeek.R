library("proto")
library("RCurl")
library("RJSONIO")
library("data.table")
library("logging")
library("Hmisc")

source("common.R")

#
# http://platform.seatgeek.com/
#
SeatGeekR <- proto(expr={
    
    baseURL <- NULL
    META_IND <- 1
    PAGE_IND <- 2
    
    # constructor
    new <- function(., baseURL="http://api.seatgeek.com/2") {
        .$proto(baseURL=baseURL)
    }
    
    # search for events
    events <- function(., ..., verbose=F) {
        .$fetch("events", ..., verbose=verbose)
    }
    
    # search for performers
    performers <- function(., ..., verbose=F) {
        .$fetch("performers", ..., verbose=verbose)
    }
    
    # search for venues
    venues <- function(., ..., verbose=F) {
        .$fetch("venues", ..., verbose=verbose)
    }
    
    # search for taxonomies
    taxonomies <- function(., ..., verbose=F) {
        .$fetch("taxonomies", ..., verbose=verbose)
    }
    
    fetch <- function(., what, ..., perPage=2000, verbose=F) {
        pages <- NULL
        pageNum <- 1
        repeat {
            # fetch a page
            result <- .$fetchPage(what, pageNum, perPage, ..., verbose=verbose)  
            page <- result[[.$PAGE_IND]]
            meta <- result[[.$META_IND]]
            pages <- append(pages, page)
            
            # how many pages are there?
            numOfPages <- ceiling(meta$total/meta$per_page)
            loginfo("fetched page %.0f/%.0f with %.0f/%.0f results", pageNum, numOfPages, length(page), meta$total)
            
            # are there more pages to fetch?
            inc(pageNum) <- 1
            if(pageNum > numOfPages) 
                break;
        }
        
        # flatten the list into a data frame
        pages <- nameList(pages, what)
        toDataTable(pages)
    }   
    
    # fetch a single page of results from the Seat Geek API
    fetchPage <- function(., what, pageNum, perPage, ..., verbose=F) {
        
        opts <- curlOptions(verbose=verbose, followlocation=T, useragent="R", ssl.verifypeer=F)
        
        # fetch the raw json
        url <- paste(.$baseURL, what, sep="/")
        json <- getForm(url, .opts=opts, page=pageNum, per_page=perPage, ...)
        
        # json to a page
        page <- fromJSON(json)
    }
})

# ensures that each list element has a unique name.
nameList <- function(aList, baseName, recursive=T) {
    
    # nothing to do
    if(length(aList)==0)
        return(aList)
    
    # if the list elements are not named, give them a unique name
    if(is.null(names(aList))) {
        names(aList) <- paste(baseName, 1:length(aList), sep=".")
    }
    
    # recursively rename any sub-lists
    if(recursive) {
        attrNames <- attributes(aList)$names
        for(i in 1:length(attrNames)) {
            attr <- aList[[i]]
            if(is.list(attr)) {
                aList[[i]] <- nameList(attr, attrNames[[i]])
            }
        }
    }
    
    return(aList)
}

