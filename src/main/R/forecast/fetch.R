
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
# validates the generated features
#
validate <- function (features, ...) {
    loginfo ("validating the feature set")
    
    # tidy up the feature set
    setkeyv (features, c("atm", "trandate"))
    setcolorder (features, neworder = c(2, 1, 3, 4:ncol (features)))
    
    # all features must be finite - two exceptions: 'usage' and future 'lags'
    finite <- is.finite (features[ !is.na(usage) ])
    if(sum(finite) < length (colnames (features)) - 1) {
        bad <- paste (names (finite)[!finite], collapse = ", ") 
        stop (sprintf ("all values must be finite: '%s'", bad))
    }
}

#
# Builds a set of features related to the date.
#
dates <- function (data) {
    loginfo ("creating date features")
    
    # add date related features
    data [, `:=`(
        atm              = ordered( atm),
        trandate         = as.Date (trandate, format="%m/%d/%Y"),
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
                      holidays.file = "holidays.csv", 
                      data.dir = "../../resources" ) {
    loginfo ("creating holiday features")
    
    # grab the raw holidays data
    holidays.raw <- read.csv(sprintf("%s/%s", data.dir, holidays.file),
                             col.names=c("date", "NULL", "holiday"),
                             colClasses=c("Date", "NULL", "character"))
    holidays <- data.table(holidays.raw, key="date")
    holidays.max <- max(holidays$date, na.rm=T)
    
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
# adds information about the dates for social security payments which may impact
# ATM activity.
#
socialSecurity <- function (features, 
                            ss.file = "social-security.csv",
                            data.dir = "../../resources") {
    loginfo("creating social security payment features")
    
    # read the social security data
    ss.path <- sprintf ("%s/%s", data.dir, ss.file)
    ss.raw <- read.csv (ss.path, colClasses = c("Date"))
    
    # create a data table
    ss <- data.table (ss.raw, social.security = TRUE, key = "date")
    
    # add in the social security pay dates
    setkeyv (features, c("trandate"))
    features [ss, social.security := social.security]
    features [is.na(social.security), social.security := FALSE]
    
    setkeyv (features, c("atm", "trandate"))
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

trendBy <- function(name, by, deposits) {
    loginfo("creating rolling trend by (%s)", eval(by))
    
    # calculate the trend **WITH TRAINING DATA ONLY**
    trend <- deposits [ 
        train == 1, 
        list ( 
            t.mean = mean.finite (usage),
            t.min  = min.finite (usage),
            t.max  = max.finite (usage),
            t.sd   = sd.finite (usage)
        ), by = by ]
    setkeyv(trend, eval(by))
    
    # merge the trend with the training data
    setkeyv(deposits, eval(by))
    deposits [trend, `:=` (mean = t.mean, min = t.min, max = t.max, sd = t.sd)]
    
    # create unique names for the trend's components
    setnames(deposits, "mean", paste(name, "mean", sep = "."))
    setnames(deposits, "min", paste(name, "min", sep = "."))
    setnames(deposits, "max", paste(name, "max", sep = "."))
    setnames(deposits, "sd", paste(name, "sd", sep = "."))
}

#
# adds a mean, min, max, sd to represent a trend by various groupings.
#
rollingTrends <- function (deposits) {
    
    # rolling trends specific to each ATM
    trendBy ("woy", by = quote (c("atm", "week.of.year")), deposits)
    trendBy ("moy", by = quote (c("atm", "month.of.year")), deposits)
    trendBy ("dow", by = quote (c("atm", "day.of.week")), deposits)
    trendBy ("wom", by = quote (c("atm", "week.of.month")), deposits)
    trendBy ("qua", by = quote (c("atm", "quarter")), deposits)
    trendBy ("hol", by = quote (c("atm", "holiday")), deposits)
    trendBy ("pay", by = quote (c("atm", "payday")), deposits)
    
    # rolling trends across the entire fleet
    trendBy ("woy.all", by = quote (c("week.of.year")), deposits)
    trendBy ("moy.all", by = quote (c("month.of.year")), deposits)
    trendBy ("dow.all", by = quote (c("day.of.week")), deposits)
    trendBy ("wom.all", by = quote (c("week.of.month")), deposits)
    trendBy ("qua.all", by = quote (c("quarter")), deposits)
    trendBy ("hol.all", by = quote (c("holiday")), deposits)
    trendBy ("pay.all", by = quote (c("payday")), deposits)
}    

#
# compute a lagged version of the usage time series by some given
# period such as day of week or month of year.
#
lagBy <- function(name, by, train, test) {
    loginfo("creating lag difference by (%s)", eval(by))
    
    # calculate the lag difference **WITH TRAINING DATA ONLY**
    lag <- train [, list ( 
        #usage,
        #lag = c(lag (usage)),
        lag.diff = c(0, diff (usage))
    ), by = c("atm", "quarter") ]
    setkeyv(lag, eval (by))
    
    # merge the trend with the training data
    setkeyv(train, eval(by))
    train [trend, `:=` (mean = t.mean, min = t.min, max = t.max, sd = t.sd)]
    
    # create unique names for the trend's components
    setnames(train, "mean", paste(name, "mean", sep = "."))
    setnames(train, "min", paste(name, "min", sep = "."))
    setnames(train, "max", paste(name, "max", sep = "."))
    setnames(train, "sd", paste(name, "sd", sep = "."))
    
    # merge the trend with the test data
    setkeyv(test, eval(by))
    test [trend, `:=` (mean = t.mean, min = t.min, max = t.max, sd = t.sd)]
    
    # create unique names for the trend's components
    setnames(test, "mean", paste(name, "mean", sep = "."))
    setnames(test, "min", paste(name, "min", sep = "."))
    setnames(test, "max", paste(name, "max", sep = "."))
    setnames(test, "sd", paste(name, "sd", sep = "."))
}



#
# calculates the lagged differences (change in usage) between different 
# intervals.  the intervals can be from one Monday to the next, from one 
# week to the next, from one quarter to the next, etc
#
lags <- function (data) {
    loginfo ("creating lagged differences")
    
    data [, lags.qua := c(0, diff (usage)), by = "quarter" ]
    data [, lags.moy := c(0, diff (usage)), by = "month.of.year" ]
    data [, lags.doy := c(0, diff (usage)), by = "day.of.year" ]
    data [, lags.dos := c(0, diff (usage)), by = "day.of.semi.year" ]
    data [, lags.doq := c(0, diff (usage)), by = "day.of.quarter" ]
    data [, lags.dom := c(0, diff (usage)), by = "day.of.month" ]
    data [, lags.dow := c(0, diff (usage)), by = "day.of.week" ]
    data [, lags.woy := c(0, diff (usage)), by = "week.of.year" ]
    data [, lags.wom := c(0, diff (usage)), by = "week.of.month" ]
    data [, lags.pay := c(0, diff (usage)), by = "payday" ]
    data [, lags.hol := c(0, diff (usage)), by = "holiday" ]
}
