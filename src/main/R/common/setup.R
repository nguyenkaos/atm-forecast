#!/usr/bin/env Rscript

##################################################################
# Installs a package only if it is not already installed.
##################################################################
install <- function(package) {
    cat(sprintf("checking for %s...\n", package))
    
    if(!require(package, character.only=T)) {
        cat(sprintf("installing %s...\n", package))
        install.packages(package, dep=T)
        
        if(!require(package, character.only=T))
            stop(sprintf("unable to install package: %s", package))
    }
}

##################################################################
# Installs all of the packages necessary for forecasting.  The
# default mirror of 81 is run by Case Western in Ohio.
##################################################################
setup <- function(mirror=81) {
    cat("installing required packages...\n")
    
    # choose a CRAN mirror site
    chooseCRANmirror(ind=mirror)
    
    # install the packages
    cat("FOO!")
    required = c("plyr", 
                 "caret", 
                 "lubridate", 
                 "gbm", 
                 "gdata", 
                 "randomForest", 
                 "Hmisc",
                 "logging",
                 "data.table",
                 "optparse")
    sapply(required, install)
    
    cat("installation successful\n")
}

Sys.setenv(HTTP_PROXY="http://proxy.jpmchase.net:8443")
setup()
