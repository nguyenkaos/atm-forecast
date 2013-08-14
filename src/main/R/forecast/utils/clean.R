

##################################################################
# Cleans and merges the input data frames and adds all of the necessary 
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
        dayOfWeek <- as.integer(wday(trandate))
        dayOfYear <- as.integer(yday(trandate))
        dayOfQuarter <- as.integer(dayOfYear %% 91)
        dayOfSemiYear <- as.integer(dayOfYear %% 182)
        weekOfMonth <- as.integer(week(trandate) - week(floor_date(trandate,"month")))
        weekOfYear <- as.integer(week(trandate))  
    })
    
    # holidays - clean
    holidays$holiday <- NULL
    holidays$date <- as.Date(holidays$date, format="%m/%d/%Y")
    holidays <- rename(holidays, c("impact"="holiday"))
    
    # holidays - merge with the cash data
    cash <- merge(x=cash, y=holidays, by.x="trandate", by.y="date", all.x=TRUE)
    cash$holidayN <- as.integer(cash$holiday)
    cash$holidayN[is.na(cash$holiday)] <- 0
    
    # pay days - need to collapse multiple pay/pre/post days into one row for each atm/date
    paydays$date <- as.Date(paydays$date, format="%m/%d/%Y")
    paydays <- subset(paydays, select=c(date, payday))
    paydays <- ddply(paydays, "date", summarise, payday = paste(payday, collapse="+"))
    
    # pay days - merge with the cash data
    cash <- merge(x=cash, y=paydays, by.x="trandate", by.y="date", all.x=TRUE)
    cash$paydayN <- as.integer(cash$payday)
    cash$paydayN[is.na(cash$paydayN)] <- 0
    
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
