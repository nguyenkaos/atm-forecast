


#
# three input data sets are required; cash, holidays and paydays
#
clean <- function(withdrawals, holidays, paydays, events) {
  
  cash <- withdrawals
  
  # preprocess the data
  cash$usage <- as.integer(cash$usage)
  
  # features related to the date
  cash$trandateN <- as.integer(cash$trandate)
  cash$dayOfWeek <- as.integer(wday(cash$trandate))
  cash$dayOfYear <- as.integer(yday(cash$trandate))
  cash$dayOfQuarter <- as.integer(cash$dayOfYear %% 91)
  cash$dayOfSemiYear <- as.integer(cash$dayOfYear %% 182)
  cash$weekOfMonth <- as.integer(week(cash$trandate) - week(floor_date(cash$trandate,"month")))
  cash$weekOfYear <- as.integer(week(cash$trandate))  
  
  # holidays
  holidays$holiday <- NULL
  holidays$date <- as.Date(holidays$date, format="%m/%d/%Y")
  holidays <- rename(holidays, c("impact"="holiday"))
  
  # merge the holidays data
  cash <- merge(x=cash, y=holidays, by.x="trandate", by.y="date", all.x=TRUE)
  cash$holidayN <- as.integer(cash$holiday)
  cash$holidayN[is.na(cash$holiday)] <- 0
  
  # pay days - collapse multiple pay/pre/post days into one row for each atm/date
  paydays$date <- as.Date(paydays$date, format="%m/%d/%Y")
  paydays <- subset(paydays, select=c(date, payday))
  paydays <- ddply(paydays, "date", summarise, payday = paste(payday, collapse="+"))
  
  # merge the pay days data
  cash <- merge(x=cash, y=paydays, by.x="trandate", by.y="date", all.x=TRUE)
  cash$paydayN <- as.integer(cash$payday)
  cash$paydayN[is.na(cash$paydayN)] <- 0
  
  # stub hub event data
  events <- rename(events, c("eventdate"="eventDate", 
                             "totalTickets"="eventTickets",
                             "distance"="eventDistance"))
  events$eventDate <- as.Date(events$eventDate, format="%m/%d/%Y")
  events <- ddply(events, c("atm","eventDate"), summarise, 
                  eventTickets = sum(eventTickets), 
                  eventDistance = mean(eventDistance))
  
  # merge the events data - collapse multiple events into 1 row for each atm/date
  cash <- merge(x=cash, y=events, all.x=TRUE, 
                by.x=c("atm","trandate"), by.y=c("atm","eventDate"))
  cash$event <- !is.na(cash$eventDistance)
  cash$eventDistance[is.na(cash$eventDistance)] <- 3000000
  
  return(cash)
}
