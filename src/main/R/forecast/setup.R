
##################################################################
# Installs all of the packages necessary for forecasting.
##################################################################
setup <- function() {
    
    # choose a CRAN mirror site
    ohio <- 81
    chooseCRANmirror(ind=ohio)
    
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
