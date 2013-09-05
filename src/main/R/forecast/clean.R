

##################################################################
# Cleans and merges the input data and creates all of the necessary 
# features.  Four input data frames are required; cash, holidays, 
# paydays, and events.  A single 'cash' data frame is returned to be used 
# for training and prediction.
##################################################################
clean <- function(libDir="../../resources") {
    
    # load the raw input data
    cash <- readRDS(sprintf("%s/withdrawals.rds", libDir))
    holidays <- read.csv(sprintf("%s/holidays.csv", libDir))
    events <- read.csv(sprintf("%s/events.csv", libDir))
    paydays <- read.csv(sprintf("%s/paydays.csv", libDir))
    
    # add date related features
    cash <- within(cash, {
        usage <- as.integer(usage)
        trandateN <- as.integer(trandate)
        quarter <- quarter(trandate)
        monthOfYear <- month(trandate)
        dayOfYear <- yday(trandate)
        dayOfSemiYear <- dayOfYear %% 182
        dayOfQuarter <- dayOfYear %% 91
        dayOfMonth <- mday(trandate)
        dayOfWeek <- wday(trandate)
        weekOfYear <- week(trandate)
        weekOfMonth <- week(trandate) - week(floor_date(trandate,"month"))
    })
    
    # trend summary over week of year
    by <- c("atm","weekOfYear")
    weekOfYear <- ddply(cash, by, summarise, woyMean=mean(usage), woyMin=min(usage), woyMax=max(usage), woySd=sd(usage))
    cash <- merge(x=cash, y=weekOfYear, by=by, all.x=T)
    
    # TODO - temporary stop gap until I can figure this out
    cash$woySd[is.na(cash$woySd)] <- 0 
    
    # trend summary over month of year
    by <- c("atm","monthOfYear")
    monthOfYear <- ddply(cash, by, summarise, moyMean=mean(usage), moyMin=min(usage), moyMax=max(usage), moySd=sd(usage))
    cash <- merge(x=cash, y=monthOfYear, by=by, all.x=T)
    
    # trend summary over day of week
    by <- c("atm","dayOfWeek")
    dayOfWeek <- ddply(cash, by, summarise, dowMean=mean(usage), dowMin=min(usage), dowMax=max(usage), dowSd=sd(usage))
    cash <- merge(x=cash, y=dayOfWeek, by=by, all.x=T)
    
    # trend summary over week of month
    by <- c("atm","weekOfMonth")
    weekOfMonth <- ddply(cash, by, summarise, womMean=mean(usage), womMin=min(usage), womMax=max(usage), womSd=sd(usage))
    cash <- merge(x=cash, y=weekOfMonth, by=by, all.x=T)
    
    # trend summary over quarter
    by <- c("atm","quarter")
    quarters <- ddply(cash, by, summarise, qMean=mean(usage), qMin=min(usage), qMax=max(usage), qSd=sd(usage))
    cash <- merge(x=cash, y=quarters, by=by, all.x=T)
    
    # TODO - trend summaries over the entire fleet instead of just a single ATM, wom, yom, etc?
    # TODO - actual usage last week
    # TODO - actual usage last month
    # TODO - actual usage last year
    
    # holidays - clean
    holidays$holiday <- NULL
    holidays$date <- as.Date(holidays$date, format="%m/%d/%Y")
    holidays <- rename(holidays, c("impact"="holiday"))
    
    # holidays - merge with the cash data
    cash <- merge(x=cash, y=holidays, by.x="trandate", by.y="date", all.x=T)
    cash$holiday <- as.character(cash$holiday)
    cash$holiday[is.na(cash$holiday)] <- "none"
    cash <- within(cash, {
        holiday <- as.factor(holiday)
        holidayN <- as.integer(holiday)
    })
    
    # trend summary by holidays
    by <- "holidayN"
    holidays <- ddply(cash, by, summarise, holMean=mean(usage), holMin=min(usage), holMax=max(usage), holSd=sd(usage))
    cash <- merge(x=cash, y=holidays, by=by, all.x=T)
    
    # pay days - need to collapse multiple pay/pre/post days into one row for each atm/date
    paydays$date <- as.Date(paydays$date, format="%m/%d/%Y")
    paydays <- subset(paydays, select=c(date, payday))
    paydays <- ddply(paydays, "date", summarise, payday = paste(payday, collapse="+"))
    
    # pay days - merge with the cash data
    cash <- merge(x=cash, y=paydays, by.x="trandate", by.y="date", all.x=TRUE)
    cash$payday[is.na(cash$payday)] <- "none"
    cash <- within(cash, {
        payday <- as.factor(payday)
        paydayN <- as.integer(payday)      
    })
    
    # trend summary by paydays
    by <- "paydayN"
    paydaySumm <- ddply(cash, by, summarise, payMean=mean(usage), payMin=min(usage), payMax=max(usage), paySd=sd(usage))
    cash <- merge(x=cash, y=paydaySumm, by=by, all.x=T)
    
    # events - clean the data gathered from stub hub
    events <- rename(events, c("eventdate"="eventDate", "totalTickets"="eventTickets", "distance"="eventDistance"))
    events$eventDate <- as.Date(events$eventDate, format="%m/%d/%Y")
    events <- ddply(events, c("atm","eventDate"), 
                    summarise, 
                    eventTickets = sum(eventTickets), 
                    eventDistance = mean(eventDistance))
    
    # events - merge with cash - collapse multiple events into 1 row for each atm/date
    cash <- merge(x=cash, y=events, all.x=TRUE, by.x=c("atm","trandate"), by.y=c("atm","eventDate"))
    cash$event <- !is.na(cash$eventDistance)
    cash$eventDistance[is.na(cash$eventDistance)] <- 3000000  
    
    return(cash)
}
