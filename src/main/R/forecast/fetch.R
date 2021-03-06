
library("lubridate")
library("logging")

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
    
    logdebug ("fetching historical usage data")
    
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
    
    setkeyv(history.complete, c("atm", "trandate"))
    return (history.complete)
}

#
# validates the generated features
#
validate <- function (features, ...) {
    loginfo ("validating the feature set")
    
    # tidy up the feature set
    setkeyv (features, c("atm", "trandate"))
    setcolorder (features, neworder = c(2, 1, 3, 4:ncol (features)))
}

#
# Builds a set of features related to the date.
#
dates <- function (data) {
    logdebug ("creating date features")
    
    # add date related features
    data [, `:=`(
        atm              = ordered (atm),
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
                      data.dir      = "../../resources" ) {
    logdebug ("creating holiday features")
    
    # grab the raw holidays data
    holidays.raw <- read.csv (file       = sprintf("%s/%s", data.dir, holidays.file),
                              col.names  = c("date", "NULL", "holiday"),
                              colClasses = c("Date", "NULL", "character"))
    
    # create a data table
    holidays <- data.table (holidays.raw, key="date")
    
    # holidays - merge with the features data
    setkeyv (features, c("trandate", "atm"))
    features [holidays,       holiday := holiday ]
    features [is.na(holiday), holiday := "none" ]
    features [,               holiday := as.factor(holiday) ]
}

#
# Fetches the paydays data and merges this with the original
# data set.
#
paydays <- function (features, 
                     paydays.file = "paydays.csv",
                     data.dir     = "../../resources") {
    logdebug ("creating payday features")
    
    # read the paydays data
    paydays.raw <- read.csv (file       = sprintf ("%s/%s", data.dir, paydays.file), 
                             col.names  = c("base", "trandate", "payday", "type"),
                             colClasses = c("NULL", "Date", "character", "NULL"))
    
    # create a data.table
    paydays <- data.table(paydays.raw, key="trandate")
    
    # collapse multiple pay/pre/post days into a single row for each (atm,date)
    paydays <- paydays [, list (
        payday = paste (unique (payday), collapse="+")
    ), by = "trandate"]
    
    # add the paydays data to the rest of the features
    setkeyv (features, c("trandate", "atm"))
    features [paydays,       payday := payday ]
    features [is.na(payday), payday := "none" ]
    features [,              payday := as.factor (payday) ]
}

#
# adds information about the dates for social security payments which may impact
# ATM activity.
#
socialSecurity <- function (features, 
                            ss.file  = "social-security.csv",
                            data.dir = "../../resources") {
    logdebug ("creating social security payment features")
    
    # read the social security data
    ss.path <- sprintf ("%s/%s", data.dir, ss.file)
    ss.raw <- read.csv (ss.path, colClasses = c("Date"))
    
    # create a data table
    ss <- data.table (ss.raw, social.security = TRUE, key = "date")
    
    # add in the social security pay dates
    setkeyv (features, c("trandate"))
    features [ss,             soc.sec := social.security]
    features [is.na(soc.sec), soc.sec := FALSE]
    
    setkeyv (features, c("atm", "trandate"))
}

#
# Adds events to the data set.
#
events <- function (features, 
                    forecast.to,
                    events.file = "events.csv",
                    data.dir    = "../../resources") {
    
    logdebug ("creating event features")
    
    # events - clean the data gathered from stub hub
    events.raw <- read.csv(file       = sprintf("%s/%s", data.dir, events.file),
                           col.names  = c("base","trandate","payday","type"),
                           colClasses = c("NULL", "Date", "character", "NULL"))
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
# Calculates a seasonal factor by some attribute.  For example, the season factor for 
# each day of the week.  If the seasonal factor for a Monday is 0.75, this can be 
# interpreted as Monday's have only 75% of volume experienced on other days.
#
seasonalFactorBy <- function(history,
                             name, 
                             by.seasonal = quote (c("atm", "day.of.week")), 
                             by.nonseasonal = quote (c("atm"))) {
    logdebug ("creating seasonal factors by (%s)", eval(by.seasonal))
    
    # calculate the average by season... also ignore 0 days which can throw off the mean
    mean.seasonal <- history [
        train == 1 & is.finite(usage) & usage > 0.0, 
        list (mean.seasonal = mean.finite (usage)), 
        by = by.seasonal ]
    
    # calculate the overall average... also ignore 0 days which can throw off the mean
    mean.nonseasonal <- history [
        train == 1 & is.finite(usage) & usage > 0.0, 
        list (mean.nonseasonal = mean.finite (usage)), 
        by = by.nonseasonal ]
    
    # calculate the seasonal factor
    factors <- mean.nonseasonal [ mean.seasonal ]
    factors [, factor := mean.seasonal / mean.nonseasonal ]
    factors [, mean.nonseasonal := NULL]
    factors [, mean.seasonal := NULL]
    setkeyv (factors, eval(by.seasonal))
    
    # merge the seasonal factor with the training data
    setkeyv (history, eval(by.seasonal))
    history [factors, eval(name) := factor]
}

#
# Calculates the mean, min, max, and sd by some attribute.  For
# example, the mean, min, max, and sd for each day of the week.
#
rollingTrendBy <- function(name, by, history) {
    logdebug ("creating rolling trend by (%s)", eval(by))
    
    # calculate the trend **WITH TRAINING DATA ONLY**
    trend <- history [ 
        train == 1, 
        list ( 
            t.mean = mean.finite (usage),
            t.min  = min.finite (usage),
            t.max  = max.finite (usage),
            t.sd   = sd.finite (usage)
        ), by = by ]
    setkeyv(trend, eval(by))
    
    # merge the trend with the training data
    setkeyv(history, eval(by))
    history [trend, `:=` (mean = t.mean, min = t.min, max = t.max, sd = t.sd)]
    
    # create unique names for the trend's components
    setnames (history, "mean", paste (name, "mean", sep = "."))
    setnames (history, "min",  paste (name, "min", sep = "."))
    setnames (history, "max",  paste (name, "max", sep = "."))
    setnames (history, "sd",   paste (name, "sd", sep = "."))
}

#
# adds a mean, min, max, sd to represent a trend by various groupings.
#
rollingTrends <- function (history) {

    rollingTrendBy ("woy", by = quote (c("atm", "week.of.year")), history)
    rollingTrendBy ("moy", by = quote (c("atm", "month.of.year")), history)
    rollingTrendBy ("dow", by = quote (c("atm", "day.of.week")), history)
    rollingTrendBy ("wom", by = quote (c("atm", "week.of.month")), history)
    rollingTrendBy ("qua", by = quote (c("atm", "quarter")), history)
    rollingTrendBy ("hol", by = quote (c("atm", "holiday")), history)
    rollingTrendBy ("pay", by = quote (c("atm", "payday")), history)
    
}    

#
# transposes recent history from a time series to a fixed width form that
# can be used for machine learning.  since forecast is at a minimum 2 weeks
# out
#
recentHistory <- function (history) {
    
    recentHistoryBy (history, "dow", quote (c ("atm", "day.of.week")))
    recentHistoryBy (history, "wom", quote (c ("atm", "week.of.month")))
    recentHistoryBy (history, "hol", quote (c ("atm", "holiday")))
    recentHistoryBy (history, "pay", quote (c ("atm", "payday")))
    recentHistoryBy (history, "soc", quote (c ("atm", "soc.sec")))
}

#
# shifts a vector over 'n' slots and back-fills with NAs
#
shift = function(vec, n) { 
    nas.to.add <- min(n, length(vec))
    c(rep (NA, nas.to.add), head (vec, -n))
}

#
# adds the previous 7 values of usage by some given period.  for example, if by = 
# c("atm", "day.of.week") a particular observation will be amended with the usage
# values for the previous 7 similar week days.  if the observation is for a Monday,
# then the most recent 7 Mondays will be added.
#
recentHistoryBy <- function (history, name, by) {
    logdebug ("creating recent history by (%s)", eval (by))
    
    history [, `:=` (
        prev.1 = shift (usage, 1),
        prev.2 = shift (usage, 2),
        prev.3 = shift (usage, 3),
        prev.4 = shift (usage, 4),
        prev.5 = shift (usage, 5),
        prev.6 = shift (usage, 6),
        prev.7 = shift (usage, 7)
    ), by = by ]
    
    # calculate the column means
    mean.prev.1 = mean (history$prev.1, na.rm = T)
    mean.prev.2 = mean (history$prev.2, na.rm = T)
    mean.prev.3 = mean (history$prev.3, na.rm = T)
    mean.prev.4 = mean (history$prev.4, na.rm = T)
    mean.prev.5 = mean (history$prev.5, na.rm = T)
    mean.prev.6 = mean (history$prev.6, na.rm = T)
    mean.prev.7 = mean (history$prev.7, na.rm = T)
    
    # replace all NAs with the column mean
    history [ is.na (prev.1), prev.1 := mean.prev.1, ]
    history [ is.na (prev.2), prev.2 := mean.prev.2, ]
    history [ is.na (prev.3), prev.3 := mean.prev.3, ]
    history [ is.na (prev.4), prev.4 := mean.prev.4, ]
    history [ is.na (prev.5), prev.5 := mean.prev.5, ]
    history [ is.na (prev.6), prev.6 := mean.prev.6, ]
    history [ is.na (prev.7), prev.7 := mean.prev.7, ]
    
    # create unique names for the trend's components
    setnames (history, "prev.1", paste(name, "usage.prev.1", sep = "."))
    setnames (history, "prev.2", paste(name, "usage.prev.2", sep = "."))
    setnames (history, "prev.3", paste(name, "usage.prev.3", sep = "."))
    setnames (history, "prev.4", paste(name, "usage.prev.4", sep = "."))
    setnames (history, "prev.5", paste(name, "usage.prev.5", sep = "."))
    setnames (history, "prev.6", paste(name, "usage.prev.6", sep = "."))
    setnames (history, "prev.7", paste(name, "usage.prev.7", sep = "."))
}

#
# the "actuals" from an ATM annot be trusted when a fault occurs.  in these
# cases the deposits history needs to be cleaned-up so that it does not disrupt
# training.  this function simply marks as NA any actuals "close to" when a fault
# occurred.  by default, caret will ignore any obs. with an NA.
#
faults <- function (deposits, faults.file = "../../resources/deposits-faults.rds") {
    
    # faults impact 'actuals' on the day of, day after, and day before a fault
    faults <- readRDS (faults.file)
    faults <- faults [, list (
        atm = atm, 
        trandate = c(trandate - 1, trandate, trandate + 1)
    )]
    
    # need a common key before merging
    setkeyv (faults,   c("atm", "trandate"))
    setkeyv (deposits, c("atm", "trandate"))
    
    # merge faults with deposits
    deposits [ faults,        fault := TRUE ]
    deposits [ is.na (fault), fault := FALSE ]

    # clear the 'usage' where a fault occured. imputation will occur during training
    deposits [ fault == T, usage := NA ]
    logdebug ("scrubbed '%s' actuals due to faults", nrow (deposits [ fault == T ])) 
}

#
# Adds a date-based sequence variable to the data set to allow for some modeling of complex
# stochastic dependencies between values.
#
sequence <- function (data) {
    data[, sequence := as.numeric(trandate)]
}
