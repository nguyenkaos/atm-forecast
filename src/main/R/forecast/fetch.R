
library("lubridate")

source("../common/cache.R")
source("../common/utils.R")

#
# in the original data set there is no record of days where there
# is no usage.  This is extremely valuable information to train
# with.  Assume that any missing days within the range of dates
# covered are zero-usage days and need to be added to the data.
#
fetch <- function( history.file,
                   forecast.to   = today() + 30,
                   data.dir      = "../../resources") {
    
    loginfo ("fetching historical usage data")
    
    # fetch the history of ATM usage; days with 0 volume are missing
    history.path <- sprintf ("%s/%s", data.dir, history.file)
    history <- data.table (readRDS (history.path), key = c("atm","trandate"))   
    
    history.min <- min (history$trandate)
    history.max <- max (history$trandate)
    history.atms <- as.character (unique (history$atm))
    
    # we 'expect' missing days in the past and future days that need forecasted
    expected <- cross.join (
        atm      = history.atms, 
        trandate = seq (history.min, forecast.to, by = "day")
    )
    
    # merge the partial history with the other dates that we expect 
    history.complete <- history [
        expected, 
        list(
            usage = if (trandate > history.max) 
                NA_real_   # indicates that a forecast is needed
            else 
                max (usage, 0, na.rm = T)
        )]
}

#
# generates the feature set from the usage history
#
generate <- function (history,                   
                      forecast.to = today() + 30, 
                      ...) {
    
    loginfo ("generating the feature set")
    features <- history
    
    dates (features, ...)
    paydays (features, forecast.to, ...)
    holidays (features, forecast.to, ...)
    localTrends (features, ...)
    globalTrends (features, ...)
    
    # TODO - need to re-engineer the events
    # events (features, ...)    
    
    features
}

#
# validates the generated features
#
validate <- function (features, ...) {
    loginfo ("validating the feature set")
    
    # tidy up the feature set
    setkeyv (features, c("atm", "trandate"))
    setcolorder (features, neworder = c(2, 1, 3, 4:ncol (features)))
    
    # only 'usage' can be NA, all others must be finite
    finite <- is.finite(features)
    if(sum(finite) < length (colnames (features)) - 1) {
        bad <- paste (names (finite)[!finite], collapse = ", ") 
        stop (sprintf ("all values must be finite: '%s'", bad))
    }
}

#
# Builds a set of features related to the date.
#
dates <- function (features) {
    loginfo ("creating date features")
    
    # add date related features
    features[, `:=`(
        atm              = ordered( atm),
        trandate         = as.Date (trandate, format="%m/%d/%Y"),
        #trandate.n       = as.integer (trandate),
        quarter          = quarter (trandate),
        month.of.year    = month (trandate),
        day.of.year      = yday (trandate),
        day.of.semi.year = yday (trandate) %% 182,
        day.of.quarter   = yday (trandate) %% 91,
        day.of.month     = mday (trandate),
        day.of.week      = wday (trandate),
        week.of.year     = week (trandate),
        week.of.month    = week (trandate) - week (floor_date (trandate, "month"))
    ),]
}

#
# Fetches the holidays data and merges this with the original
# data set.
#
holidays <- function (features, 
                      forecast.to,
                      holidays.file = "holidays.csv", 
                      data.dir = "../../resources" ) {
    loginfo ("creating holiday features")
    
    # grab the raw holidays data
    holidays.raw <- read.csv(sprintf("%s/%s", data.dir, holidays.file),
                             col.names=c("date", "NULL", "holiday"),
                             colClasses=c("Date", "NULL", "character"))
    holidays <- data.table(holidays.raw, key="date")
    holidays.max <- max(holidays$date, na.rm=T)
    
    # ensure that the holidays data set is complete
    if(holidays.max < forecast.to)
        stop (sprintf ("unable to forecast; holidays data only found up to %s", holidays.max))
    
    # holidays - merge with the features data
    setkeyv(features, c("trandate", "atm"))
    features[holidays, holiday := holiday]
    features[is.na(holiday), holiday := "none"]
    features[, holiday := as.factor(holiday)]
}

#
# Fetches the paydays data and merges this with the original
# data set.
#
paydays <- function (features, 
                     forecast.to,
                     paydays.file = "paydays.csv",
                     data.dir     = "../../resources") {
    loginfo ("creating payday features")
    
    # read the paydays data
    paydays.path <- sprintf("%s/%s", data.dir, paydays.file)
    paydays.raw <- read.csv(paydays.path, 
                            col.names=c("base", "trandate", "payday", "type"),
                            colClasses=c("NULL", "Date", "character", "NULL"))
    
    # create a data.table
    paydays <- data.table(paydays.raw, key="trandate")
    paydays.max <- max(paydays$trandate, na.rm=T)
    
    # ensure that the holidays data set is complete
    if(paydays.max < forecast.to)
        stop (sprintf ("unable to forecast; paydays data only found up to %s", paydays.max))
    
    # collapse multiple pay/pre/post days into a single row for each (atm,date)
    paydays <- paydays [, list (
        payday = paste (unique (payday), collapse="+")
    ), by = "trandate"]
    
    # add the paydays data to the rest of the features
    setkeyv (features, c("trandate", "atm"))
    features [paydays, payday := payday]
    features [is.na(payday), payday := "none"]
    features [, payday := as.factor (payday)]
}

#
# Adds events to the data set.
#
events <- function (features, 
                    forecast.to,
                    events.file = "events.csv",
                    data.dir    = "../../resources") {
    
    loginfo ("creating event features")
    
    # events - clean the data gathered from stub hub
    events.raw <- read.csv(sprintf("%s/%s", data.dir, events.file),
                           col.names=c("base","trandate","payday","type"),
                           colClasses=c("NULL", "Date", "character", "NULL"))
    events <- rename(events, c("eventdate"    = "trandate", 
                               "totalTickets" = "eventTickets", 
                               "distance"     = "eventDistance"))
    events$trandate <- as.Date(events$trandate, format="%m/%d/%Y")
    events <- ddply(events, 
                    c("atm","trandate"), 
                    summarise, 
                    eventTickets = sum(eventTickets), 
                    eventDistance = mean.finite(eventDistance, default=3000000))
    
    # events - merge with features - collapse multiple events into 1 row for each atm/date
    events <- data.table(events, key="trandate")
    features <- merge(x=features, y=events, all.x=TRUE, by=c("atm","trandate"))
    
    return(features)
}

#
# adds a mean, min, max, sd to represent a trend by various groupings.
#
localTrends <- function (data) {
    
    loginfo ("creating trend by (atm, week.of.year)")
    data[,`:=`( woy.mean = mean.finite(usage),
#                woy.min = min.finite(usage),
                woy.max = max.finite(usage),
                woy.sd = sd.finite(usage)),
         by = list (atm, week.of.year)]
    
    loginfo ("creating trend by (atm, month.of.year)")
    data[,`:=`( moy.mean = mean.finite(usage),
#                moy.min = min.finite(usage),
                moy.max = max.finite(usage),
                moy.sd = sd.finite(usage)),
         by = list (atm, month.of.year)]   
    
    loginfo ("creating trend by (atm, day.of.week)")
    data[,`:=`( dow.mean = mean.finite(usage),
#                dow.min = min.finite(usage),
                dow.max = max.finite(usage),
                dow.sd = sd.finite(usage)),
         by = list (atm, day.of.week)]  
    
    loginfo ("creating trend by (atm, week.of.month)")
    data[,`:=`( wom.mean = mean.finite(usage),
 #               wom.min = min.finite(usage),
                wom.max = max.finite(usage),
                wom.sd = sd.finite(usage)),
         by = list (atm, week.of.month)]  
    
    loginfo ("creating trend by (atm, quarter)")
    data[,`:=`( qua.mean = mean.finite(usage),
    #            qua.min = min.finite(usage),
                qua.max = max.finite(usage),
                qua.sd = sd.finite(usage)),
         by = list (atm, quarter)] 
    
    loginfo ("creating trend by (atm, holiday)")
    data[,`:=`( hol.mean = mean.finite(usage),
     #           hol.min = min.finite(usage),
                hol.max = max.finite(usage),
                hol.sd = sd.finite(usage)),
         by = list (atm, holiday)] 
    
    loginfo ("creating trend by (atm, payday)")
    data[,`:=`( pay.mean = mean.finite(usage),
      #          pay.min = min.finite(usage),
                pay.max = max.finite(usage),
                pay.sd = sd.finite(usage)),
         by = list (atm, payday)]         
}    

#
# adds a mean, min, max, sd to represent a trend by various groupings.
#
globalTrends <- function (data) {
    
    loginfo ("creating trend by week.of.year")
    data[,`:=`( woy.all.mean = mean.finite(usage),
                woy.all.min = min.finite(usage),
                woy.all.max = max.finite(usage),
                woy.all.sd = sd.finite(usage)),
         by = week.of.year]   
    
    loginfo ("creating trend by month.of.year")
    data[,`:=`( moy.all.mean = mean.finite(usage),
                moy.all.min = min.finite(usage),
                moy.all.max = max.finite(usage),
                moy.all.sd = sd.finite(usage)),
         by = month.of.year]   
    
    loginfo ("creating trend by day.of.week")
    data[,`:=`( dow.all.mean = mean.finite(usage),
                dow.all.min = min.finite(usage),
                dow.all.max = max.finite(usage),
                dow.all.sd = sd.finite(usage)),
         by = day.of.week]   
    
    loginfo ("creating trend by week.of.month")
    data[,`:=`( wom.all.mean = mean.finite(usage),
                wom.all.min = min.finite(usage),
                wom.all.max = max.finite(usage),
                wom.all.sd = sd.finite(usage)),
         by = week.of.month]   
    
    loginfo ("creating trend by quarter")
    data[,`:=`( qua.all.mean = mean.finite(usage),
                qua.all.min = min.finite(usage),
                qua.all.max = max.finite(usage),
                qua.all.sd = sd.finite(usage)),
         by = quarter]   
    
    loginfo ("creating trend by holiday")
    data[,`:=`( hol.all.mean = mean.finite(usage),
                hol.all.min = min.finite(usage),
                hol.all.max = max.finite(usage),
                hol.all.sd = sd.finite(usage)),
         by = holiday]   
    
    loginfo ("creating trend by payday")
    data[,`:=`( pay.all.mean = mean.finite(usage),
                pay.all.min = min.finite(usage),
                pay.all.max = max.finite(usage),
                pay.all.sd = sd.finite(usage)),
         by = payday]   
}
