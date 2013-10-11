
##################################################################
# Builds a set of features related to the date.
##################################################################
addDateFeatures <- function(cash) {
    
    # add date related features
    cash[, `:=`(
        atm = ordered(atm),
        trandate = as.Date(trandate, format="%m/%d/%Y"),
        trandate.n = as.integer(trandate),
        quarter = quarter(trandate),
        month.of.year = month(trandate),
        day.of.year = yday(trandate),
        day.of.semi.year = yday(trandate) %% 182,
        day.of.quarter = yday(trandate) %% 91,
        day.of.month = mday(trandate),
        day.of.week = wday(trandate),
        week.of.year = week(trandate),
        week.of.month = week(trandate) - week(floor_date(trandate,"month"))
    ),]
}

##################################################################
# Fetches the holidays data and merges this with the original
# data set.
##################################################################
addHolidays <- function(cash, holidaysFile, dataDir, forecastTo) {
    
    # holidays - clean
    holidays <- read.csv(sprintf("%s/%s", dataDir, holidaysFile))
    holidays$holiday <- NULL
    holidays$date <- as.Date(holidays$date)
    holidays <- rename(holidays, c("date"="trandate", "impact"="holiday"))
    holidays <- data.table(holidays, key="trandate")
    
    # ensure that the holidays data set is complete
    if(max(holidays$trandate, na.rm=T) < forecastTo)
        stop(sprintf("missing holidays data up to %s", forecastTo))
    
    # holidays - merge with the cash data
    cash <- merge(x=cash, y=holidays, by="trandate", all.x=T)
    cash$holiday <- as.character(cash$holiday)
    cash$holiday[is.na(cash$holiday)] <- "none"
    cash <- within(cash, {
        holiday <- as.factor(holiday)
        holiday.n <- as.integer(holiday)
    })
    
    return(cash)
}

default <- function(value, default) {
    value[is.na(value)] <- default
    return(value)
}

##################################################################
# Fetches the paydays data and merges this with the original
# data set.
##################################################################
addPaydays <- function(cash, paydaysFile, dataDir, forecastTo) {
    
    # read the paydays into a data.table
    paydays <- data.table(
        read.csv(sprintf("%s/%s", dataDir, paydaysFile), 
                 col.names=c("base","trandate","payday","type"),
                 colClasses=c("NULL", "Date", "character", "NULL"),
                 stringsAsFactors=FALSE),
        key="trandate")

    # ensure that the holidays data set is complete
    if(max(paydays$trandate, na.rm=T) < forecastTo)
        stop(sprintf("missing paydays data up to %s", forecastTo))
    
    # collapse multiple pay/pre/post days into a single row for each (atm,date)
    paydays <- paydays[, 
                       list(payday = paste(unique(payday), collapse="+")), 
                       by="trandate"]

    # add the paydays data to the rest of the features
    setkeyv(cash, c("trandate", "atm"))
    cash[paydays, payday := default(payday, "none"), ]
    
    # TODO - I shouldnt have to do this - default(..) should take care of this
    cash$payday[is.na(cash$payday)] <- "none"
    
    # TODO - is there a cleaner way to do this??
    cash <- within(cash, {
        payday <- as.factor(payday)
        payday.n <- as.numeric(payday)
    })
}

addEvents <- function(cash, eventsFile, dataDir) {
    
    # events - clean the data gathered from stub hub
    events <- read.csv(sprintf("%s/%s", dataDir, eventsFile))
    events <- rename(events, c("eventdate"    = "trandate", 
                               "totalTickets" = "eventTickets", 
                               "distance"     = "eventDistance"))
    events$trandate <- as.Date(events$trandate, format="%m/%d/%Y")
    events <- ddply(events, 
                    c("atm","trandate"), 
                    summarise, 
                    eventTickets = sum(eventTickets), 
                    eventDistance = mean(eventDistance))
    
    # events - merge with cash - collapse multiple events into 1 row for each atm/date
    events <- data.table(events, key="trandate")
    cash <- merge(x=cash, y=events, all.x=TRUE, by=c("atm","trandate"))
    cash$eventDistance[is.na(cash$eventDistance)] <- 3000000  
    
    return(cash)
}

##################################################################
# Adds a mean, min, max, sd to represent a trend by various 
# groupings.
##################################################################
addTrends <- function(data) {
    loginfo("creating trends", by)
    
    # trend by (atm, week.of.year)
    data[,`:=`( woy.mean = mean(usage, na.rm=T),
                woy.min = min(usage, na.rm=T),
                woy.max = max(usage, na.rm=T),
                woy.sd = sd(usage, na.rm=T)),
         by = list(atm, week.of.year)]
    
    # trend by (atm, month.of.year)
    data[,`:=`( moy.mean = mean(usage, na.rm=T),
                moy.min = min(usage, na.rm=T),
                moy.max = max(usage, na.rm=T),
                moy.sd = sd(usage, na.rm=T)),
         by = list(atm, month.of.year)]   
    
    # trend by (atm, day.of.week)
    data[,`:=`( dow.mean = mean(usage, na.rm=T),
                dow.min = min(usage, na.rm=T),
                dow.max = max(usage, na.rm=T),
                dow.sd = sd(usage, na.rm=T)),
         by = list(atm, day.of.week)]  
    
    # trend by (atm, week.of.month)
    data[,`:=`( wom.mean = mean(usage, na.rm=T),
                wom.min = min(usage, na.rm=T),
                wom.max = max(usage, na.rm=T),
                wom.sd = sd(usage, na.rm=T)),
         by = list(atm, week.of.month)]  
    
    # trend by (atm, quarter)
    data[,`:=`( qua.mean = mean(usage, na.rm=T),
                qua.min = min(usage, na.rm=T),
                qua.max = max(usage, na.rm=T),
                qua.sd = sd(usage, na.rm=T)),
         by = list(atm, quarter)] 
    
    # trend by (atm, holiday.n)
    data[,`:=`( hol.mean = mean(usage, na.rm=T),
                hol.min = min(usage, na.rm=T),
                hol.max = max(usage, na.rm=T),
                hol.sd = sd(usage, na.rm=T)),
         by = list(atm, holiday.n)] 
    
    # trend by (atm, payday.n)
    data[,`:=`( pay.mean = mean(usage, na.rm=T),
                pay.min = min(usage, na.rm=T),
                pay.max = max(usage, na.rm=T),
                pay.sd = sd(usage, na.rm=T)),
         by = list(atm, payday.n)]         
    
    # trend by week.of.year
    data[,`:=`( woy.all.mean = mean(usage, na.rm=T),
                woy.all.min = min(usage, na.rm=T),
                woy.all.max = max(usage, na.rm=T),
                woy.all.sd = sd(usage, na.rm=T)),
         by = week.of.year]   
    
    # trend by month.of.year
    data[,`:=`( moy.all.mean = mean(usage, na.rm=T),
                moy.all.min = min(usage, na.rm=T),
                moy.all.max = max(usage, na.rm=T),
                moy.all.sd = sd(usage, na.rm=T)),
         by = month.of.year]   
    
    # trend by day.of.week
    data[,`:=`( dow.all.mean = mean(usage, na.rm=T),
                dow.all.min = min(usage, na.rm=T),
                dow.all.max = max(usage, na.rm=T),
                dow.all.sd = sd(usage, na.rm=T)),
         by = day.of.week]   
    
    # trend by week.of.month
    data[,`:=`( wom.all.mean = mean(usage, na.rm=T),
                wom.all.min = min(usage, na.rm=T),
                wom.all.max = max(usage, na.rm=T),
                wom.all.sd = sd(usage, na.rm=T)),
         by = week.of.month]   
    
    # trend by quarter
    data[,`:=`( qua.all.mean = mean(usage, na.rm=T),
                qua.all.min = min(usage, na.rm=T),
                qua.all.max = max(usage, na.rm=T),
                qua.all.sd = sd(usage, na.rm=T)),
         by = quarter]   
    
    # trend by holiday.n
    data[,`:=`( hol.all.mean = mean(usage, na.rm=T),
                hol.all.min = min(usage, na.rm=T),
                hol.all.max = max(usage, na.rm=T),
                hol.all.sd = sd(usage, na.rm=T)),
         by = holiday.n]   
    
    # trend by payday.n
    data[,`:=`( pay.all.mean = mean(usage, na.rm=T),
                pay.all.min = min(usage, na.rm=T),
                pay.all.max = max(usage, na.rm=T),
                pay.all.sd = sd(usage, na.rm=T)),
         by = payday.n]   
}

##################################################################
# In the original data set there is no record of days where there
# is no usage.  This is extremely valuable information to train
# with.  Assume that any missing days within the range of dates
# covered are zero-usage days and need to be added to the data.
##################################################################
fetchCash <- function(filename, forecastTo) {
    
    # fetch the history of ATM usage; days with 0 volume are missing
    history <- data.table(readRDS(filename), key=c("atm","trandate"))   
    
    # generate all atm/day records that should exist
    all <- CJ(
        atm = as.character(unique(history$atm)), 
        trandate = seq(min(history$trandate), max(history$trandate), by="day"),
        sorted = T
    )
    
    # a merge results in a 'complete' history; no missing days
    history[all, list(usage = max(usage, 0, na.rm=T)), ]
    
    # which 'future' dates need to be forecasted?  CJ is a fast version of expand.matrix
    future <- CJ(atm=unique(history$atm), 
                 trandate=seq(from=max(history$trandate)+1, to=forecastTo, by="days"), 
                 usage=NA_integer_)
    
    # combine the historical data with what needs forecasted
    cash <- rbindlist(list(history, future))
}

##################################################################
# Cleans and merges the input data and creates all of the necessary 
# features.  Four input data frames are required; cash, holidays, 
# paydays, and events.  A single 'cash' data frame is returned to be used 
# for training and prediction.
##################################################################
fetch <- function(forecastTo   = today()+30,
                  dataDir      = "../../resources",
                  usageFile    = "usage-micro.rds",
                  holidaysFile = "holidays.csv",
                  eventsFile   = "events.csv",
                  paydaysFile  = "paydays.csv") {
    
    cash <- cache("cash", {
        cash <- fetchCash(sprintf("%s/%s", dataDir, usageFile), forecastTo)
        addDateFeatures(cash)
        cash <- addPaydays(cash, paydaysFile, dataDir, forecastTo)
        cash <- addHolidays(cash, holidaysFile, dataDir, forecastTo)
        #cash <- addEvents(cash, eventsFile, dataDir)        
        addTrends(cash)
        
        # tidy up a bit
        cash <- subset(cash, select=c(8:10,1:7,11:ncol(cash)))
        setkeyv(cash, c("atm", "trandate"))
    })
    
    # sanity check - only 'usage' can be NA, all others must be finite
    cashNoUsage <- subset(cash, select=-c(usage))
    finite <- is.finite(cashNoUsage)
    if(!all(finite)) {
        stop(sprintf("All values must be finite!  Check %s", 
                     paste(names(finite)[!finite], collapse=", ")))
    }
    
    return(cash)
}


##################################################################
# Checks all columns/features in a data frame to ensure
# they are 'finite' meaning not NA, NULL, NaN, etc.
##################################################################
is.finite.data.frame <- function(df){
    sapply(df, function(col) all(is.finite(col)))
}


