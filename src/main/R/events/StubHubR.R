library(proto)
library(RCurl)
library(RJSONIO)

StubHubR <- proto(expr={
    
    baseURL <- NULL
    scope <- NULL
    token <- NULL
    
    # constructor
    new <- function(self, baseURL="https://api.stubhub.com/", scope="PRODUCTION") {
        self$proto(baseURL=baseURL, scope=scope)
    }
    
    # login to the stub hub api
    login <- function(self, key, secret, username, password, verbose=F) { 
        credentials <- base64(sprintf("%s:%s", key, secret))
        json <- postForm(paste0(self$baseURL, "login"), 
                         style="post", 
                         .opts=curlOptions(
                             verbose=verbose, 
                             ssl.verifypeer=F,
                             httpheader=sprintf("Authorization: Basic %s", credentials)), 
                         grant_type="password",
                         username=username,
                         password=password,
                         scope=self$scope) 
        self$token <- fromJSON(json)
    }
    
    searchInventory <- function(self, verbose=F) {
        if(is.null(self$token)) {
            stop("missing auth token. did you login?")
        }
        
        json <- getForm(paste0(self$baseURL, "catalog/events/v1"), 
                        event="Concert",
                       .opts=curlOptions(
                           verbose=verbose, 
                           ssl.verifypeer=F,
                           httpheader=sprintf("Authorization: Bearer %s", self$token$access_token)))         
    }
})




