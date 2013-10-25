
library("plyr")
library("data.table")

#
# transforms the historical usage data (either deposits or withdrawals) into a
# time series.
#
as.ts <- function(history) {
    
    # tranform the 'long' history to a 'wide' time series
    history [, list(
        time.series = list( rev ( usage [!is.na (usage)]))
    ), by = atm ]
}