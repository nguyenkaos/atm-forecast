
library("logging")
library("data.table")
library("lubridate")

source("../common/cache.R")
source("../common/utils.R")

basicConfig(level=loglevels["INFO"])

fetch <- function(file="../../resources/deposits.rds") {
    
    # build a feature set indexed by (atm, date)
    deposits <- readRDS(file)
    addDates(deposits)
    addHolidays(deposits)
    addPaydays(deposits)
    addTrends(deposits)
    
    # collapse the feature set so that it is indexed by (atm) only
}

addDates <- function(deposits) {
    loginfo("creating date features")
    deposits[, `:=`(
        date.n = as.integer(date),
        quarter = quarter(date),
        month.of.year = month(date),
        day.of.year = yday(date),
        day.of.semi.year = yday(date) %% 182,
        day.of.quarter = yday(date) %% 91,
        day.of.month = mday(date),
        day.of.week = wday(date),
        week.of.year = week(date),
        week.of.month = week(date) - week(floor_date(date,"month"))
    ),]
}

#
# Fetches the holidays data and merges this with the original
# data set.
#
addHolidays <- function(deposits, forecast.to=30, holidays.file="holidays.csv", data.dir="../../resources") {
    loginfo("creating holiday features")
    
    # grab the raw holidays data
    holidays.raw <- read.csv(sprintf("%s/%s", data.dir, holidays.file),
                             col.names=c("date", "NULL", "holiday"),
                             colClasses=c("Date", "NULL", "character"))
    holidays <- data.table(holidays.raw, key="date")
    holidays.max <- max(holidays$date, na.rm=T)
    
    # ensure that the holidays data set is complete
    if(holidays.max < forecast.to)
        stop(sprintf("holidays data required up to %s, but only found up to %s", forecast.to, holidays.max))
    
    # holidays - merge with the deposits data
    setkeyv(deposits, c("date", "atm"))
    deposits[holidays, holiday := holiday]
    deposits[is.na(holiday), holiday := "none"]
    deposits[,`:=`(
        holiday = as.factor(holiday),
        holiday.n = as.numeric(as.factor(holiday))
    )]
}

#
# Fetches the paydays data and merges this with the original
# data set.
#
addPaydays <- function(deposits, forecast.to=30, paydays.file="paydays.csv", data.dir="../../resources") {
    loginfo("creating payday features")
    
    # read the paydays into a data.table
    paydays.raw <- read.csv(sprintf("%s/%s", data.dir, paydays.file), 
                            col.names=c("base","date","payday","type"),
                            colClasses=c("NULL", "Date", "character", "NULL"))
    paydays <- data.table(paydays.raw, key="date")
    paydays.max <- max(paydays$date, na.rm=T)
    
    # ensure that the holidays data set is complete
    if(paydays.max < forecast.to)
        stop(sprintf("paydays data required up to %s, but only found up to %s", forecast.to, paydays.max))
    
    # collapse multiple pay/pre/post days into a single row for each (atm,date)
    paydays <- paydays[, 
                       list(payday = paste(unique(payday), collapse="+")), 
                       by="date"]
    
    # add the paydays data to the rest of the features
    setkeyv(deposits, c("date", "atm"))
    deposits[paydays, payday := payday]
    deposits[is.na(payday), payday:="none"]
    deposits[,`:=`(
        payday = as.factor(payday),
        payday.n = as.numeric(as.factor(payday))
    )]
}


#
# Adds a mean, min, max, sd to represent a trend by various 
# groupings.
#
addTrends <- function(data) {
    
    loginfo("creating trend by (atm, week.of.year)")
    data[,`:=`( woy.mean = mean.finite(usage),
                woy.min = min.finite(usage),
                woy.max = max.finite(usage),
                woy.sd = sd.finite(usage)),
         by = list(atm, week.of.year)]
    
    loginfo("creating trend by (atm, month.of.year)")
    data[,`:=`( moy.mean = mean.finite(usage),
                moy.min = min.finite(usage),
                moy.max = max.finite(usage),
                moy.sd = sd.finite(usage)),
         by = list(atm, month.of.year)]   
    
    loginfo("creating trend by (atm, day.of.week)")
    data[,`:=`( dow.mean = mean.finite(usage),
                dow.min = min.finite(usage),
                dow.max = max.finite(usage),
                dow.sd = sd.finite(usage)),
         by = list(atm, day.of.week)]  
    
    loginfo("creating trend by (atm, week.of.month)")
    data[,`:=`( wom.mean = mean.finite(usage),
                wom.min = min.finite(usage),
                wom.max = max.finite(usage),
                wom.sd = sd.finite(usage)),
         by = list(atm, week.of.month)]  
    
    loginfo("creating trend by (atm, quarter)")
    data[,`:=`( qua.mean = mean.finite(usage),
                qua.min = min.finite(usage),
                qua.max = max.finite(usage),
                qua.sd = sd.finite(usage)),
         by = list(atm, quarter)] 
    
    loginfo("creating trend by (atm, holiday.n)")
    data[,`:=`( hol.mean = mean.finite(usage),
                hol.min = min.finite(usage),
                hol.max = max.finite(usage),
                hol.sd = sd.finite(usage)),
         by = list(atm, holiday.n)] 
    
    loginfo("creating trend by (atm, payday.n)")
    data[,`:=`( pay.mean = mean.finite(usage),
                pay.min = min.finite(usage),
                pay.max = max.finite(usage),
                pay.sd = sd.finite(usage)),
         by = list(atm, payday.n)]         
    
    loginfo("creating trend by week.of.year")
    data[,`:=`( woy.all.mean = mean.finite(usage),
                woy.all.min = min.finite(usage),
                woy.all.max = max.finite(usage),
                woy.all.sd = sd.finite(usage)),
         by = week.of.year]   
    
    loginfo("creating trend by month.of.year")
    data[,`:=`( moy.all.mean = mean.finite(usage),
                moy.all.min = min.finite(usage),
                moy.all.max = max.finite(usage),
                moy.all.sd = sd.finite(usage)),
         by = month.of.year]   
    
    loginfo("creating trend by day.of.week")
    data[,`:=`( dow.all.mean = mean.finite(usage),
                dow.all.min = min.finite(usage),
                dow.all.max = max.finite(usage),
                dow.all.sd = sd.finite(usage)),
         by = day.of.week]   
    
    loginfo("creating trend by week.of.month")
    data[,`:=`( wom.all.mean = mean.finite(usage),
                wom.all.min = min.finite(usage),
                wom.all.max = max.finite(usage),
                wom.all.sd = sd.finite(usage)),
         by = week.of.month]   
    
    loginfo("creating trend by quarter")
    data[,`:=`( qua.all.mean = mean.finite(usage),
                qua.all.min = min.finite(usage),
                qua.all.max = max.finite(usage),
                qua.all.sd = sd.finite(usage)),
         by = quarter]   
    
    loginfo("creating trend by holiday.n")
    data[,`:=`( hol.all.mean = mean.finite(usage),
                hol.all.min = min.finite(usage),
                hol.all.max = max.finite(usage),
                hol.all.sd = sd.finite(usage)),
         by = holiday.n]   
    
    loginfo("creating trend by payday.n")
    data[,`:=`( pay.all.mean = mean.finite(usage),
                pay.all.min = min.finite(usage),
                pay.all.max = max.finite(usage),
                pay.all.sd = sd.finite(usage)),
         by = payday.n]   
}