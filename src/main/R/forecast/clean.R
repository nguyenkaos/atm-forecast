
##################################################################
# Adds a mean, min, max, sd when aggregated by the 'by' argument
# to a data frame.
##################################################################
trendSummary <- function(data, by, abbrev) {
  summary <- ddply(data, by, summarise, 
                   mean=mean(usage, na.rm=T), 
                   min=min(usage, na.rm=T), 
                   max=max(usage, na.rm=T), 
                   sd=sd(usage, na.rm=T))
  
  # ensure that there are no unexpected NAs
  summary$mean[is.na(summary$mean)] <- 0
  summary$min[is.na(summary$min)] <- 0
  summary$max[is.na(summary$max)] <- 0
  summary$sd[is.na(summary$sd)] <- 0
  
  # alter the column names
  names(summary) <- c(by, 
                      paste0(abbrev,"Mean"), 
                      paste0(abbrev,"Min"),
                      paste0(abbrev,"Max"),
                      paste0(abbrev,"Sd"))
  data <- merge(x=data, y=summary, by=by, all.x=T)
  return(data)
}

##################################################################
# Cleans and merges the input data and creates all of the necessary 
# features.  Four input data frames are required; cash, holidays, 
# paydays, and events.  A single 'cash' data frame is returned to be used 
# for training and prediction.
##################################################################
clean <- function(libDir="../../resources",
                  usage="usage.rds",
                  holidays="holidays.csv",
                  events="events.csv",
                  paydays="paydays.csv") {
  
  # load the raw input data
  cash <- readRDS(sprintf("%s/%s", libDir, usage))
  holidays <- read.csv(sprintf("%s/%s", libDir, holidays))
  events <- read.csv(sprintf("%s/%s", libDir, events))
  paydays <- read.csv(sprintf("%s/%s", libDir, paydays))
  
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
  
  # events - clean the data gathered from stub hub
  events <- rename(events, c("eventdate"="eventDate", "totalTickets"="eventTickets", "distance"="eventDistance"))
  events$eventDate <- as.Date(events$eventDate, format="%m/%d/%Y")
  events <- ddply(events, c("atm","eventDate"), 
                  summarise, 
                  eventTickets = sum(eventTickets), 
                  eventDistance = mean(eventDistance))
  
  # events - merge with cash - collapse multiple events into 1 row for each atm/date
  cash <- merge(x=cash, y=events, all.x=TRUE, by.x=c("atm","trandate"), by.y=c("atm","eventDate"))
  #cash$event <- !is.na(cash$eventDistance)
  cash$eventDistance[is.na(cash$eventDistance)] <- 3000000  
  
  # add trend summaries specific to the ATM
  cash <- trendSummary(cash, by=c("atm","weekOfYear"), "woy")
  cash <- trendSummary(cash, by=c("atm","monthOfYear"), "moy")
  cash <- trendSummary(cash, by=c("atm","dayOfWeek"), "dow")
  cash <- trendSummary(cash, by=c("atm","weekOfMonth"), "wom")
  cash <- trendSummary(cash, by=c("atm","quarter"), "qua")
  cash <- trendSummary(cash, by=c("atm","holidayN"), "hol")
  cash <- trendSummary(cash, by=c("atm","paydayN"), "pay")  
  
  # add trend summaries across all of the ATMs
  cash <- trendSummary(cash, by="weekOfYear", "woyAll")
  cash <- trendSummary(cash, by="monthOfYear", "moyAll")
  cash <- trendSummary(cash, by="dayOfWeek", "dowAll")
  cash <- trendSummary(cash, by="weekOfMonth", "womAll")
  cash <- trendSummary(cash, by="quarter", "quaAll")  
  cash <- trendSummary(cash, by="holidayN", "holAll")
  cash <- trendSummary(cash, by="paydayN", "payAll")
  
  return(cash)
}
