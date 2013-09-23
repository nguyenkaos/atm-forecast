library(proto)
library(RCurl)
library(RJSONIO)
library(data.table)
library(logging)

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
        self$fetch("events", ..., verbose=verbose)
    }
    
    # search for performers
    performers <- function(self, ..., verbose=F) {
        self$fetch("performers", ..., verbose=verbose)
    }
    
    # search for venues
    venues <- function(self, ..., verbose=F) {
        self$fetch("venues", ..., verbose=verbose)
    }
    
    # search for taxonomies
    taxonomies <- function(self, ..., verbose=F) {
        self$fetch("taxonomies", ..., verbose=verbose)
    }
    
    fetch <- function(self, what, ..., perPage=20, verbose=F) {
        # fetch the first page
        result <- self$fetchPage(what, 1, perPage, ..., verbose=verbose)  
        page1 <- result[[2]]
        loginfo("page 1 has %.0f results", length(page1))
        
        # how many pages are there?
        meta <- result[[1]]
        total <- meta$total
        perPage <- meta$per_page
        numberOfPages <- ceiling(total/perPage)
        loginfo("there are %.0f total across %.0f pages", total, numberOfPages)
        
        # fetch the remainder of the pages, if required
        page2Plus <- NULL
        if(numberOfPages > 1) {
            page2Plus <- sapply(2:numberOfPages, function(pageNum) {
                page <- self$fetchPage(what, pageNum, perPage, ..., verbose=verbose)[[2]]
                loginfo("page %.0f has %.0f results", pageNum, length(page))
                return(page)
            })
        }
        
        # page2Plus is a list of lists - each sub-list is for the page
        # page1 is just one list
        # how to merge these at the same level?
        append(page1, unlist(page2Plus))
    }   
    
    #
    # Returns a single page of results from the Seat Geek API.
    #
    fetchPage <- function(self, what, pageNum, perPage, ..., verbose=F) {
        loginfo("fetching page %.0f with %.0f results per page", pageNum, perPage)
        
        # fetch a page of results
        url <- paste(self$baseURL, what, sep="/")
        json <- getForm(url, .opts=curlOptions(verbose=verbose), page=pageNum, per_page=perPage, ...)
        page <- fromJSON(json)
        
        # ensure that element has a unique name
        page[[2]] <- nameEach(page[[2]], what)
        page
    }
})

##
# Ensures that each list element has a unique name.
#
nameEach <- function(aList, baseName, recursive=T) {
    
    # nothing to do; also ends recursion
    if(length(aList)==0)
        return(aList)
    
    # if the list elements are not named, give them a unique name
    if(is.null(names(aList))) {
        names(aList) <- paste(baseName, 1:length(aList), sep=".")
    }
    
    if(recursive) {
        # recursively rename any sub-lists
        attrNames <- attributes(aList)$names
        for(i in 1:length(attrNames)) {
            attr <- aList[[i]]
            if(is.list(attr)) {
                aList[[i]] <- nameEach(attr, attrNames[[i]])
            }
        }
    }
    
    return(aList)
}

##
# 
# 
toDataTable <- function(aList) {
    matrix <- do.call(rbind.fill.matrix, lapply(aList, function(l) t(unlist(l))))
    data.table(matrix)        
}

