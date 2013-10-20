
library("plyr")
library("data.table")

#
# transforms the historical usage data (either deposits or withdrawals) into a
# time series.
#
as.ts <- function(history) {
    
    # tranform the 'long' history to a 'wide' time series
    history.ts <- dlply( history, 
                         "atm", 
                         function(byAtm) { 
                             rev(as.vector(byAtm$usage)) 
                         }, 
                         .progress = "text")
    
    # time series will be of different lengths; need most recent data 
    # first to allow ATMs with different life spans to be compared
    history.dt <- data.table(atm = names(history.ts), time.series = I(history.ts), key="atm")
}