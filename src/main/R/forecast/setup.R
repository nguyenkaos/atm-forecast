
##################################################################
# Installs all of the packages necessary for forecasting.  The
# default mirror of 81 is run by Case Western in Ohio.
##################################################################
setup <- function(mirror=81) {
    
    # choose a CRAN mirror site
    chooseCRANmirror(ind=mirror)
    
    # install the packages
    install.packages(c(
        "plyr", 
        "caret", 
        "lubridate", 
        "gbm", 
        "gdata", 
        "randomForest", 
        "Hmisc",
        "logging"))
}

setup()
