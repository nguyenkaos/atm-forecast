
##################################################################
# Builds a set of features related to the date.
##################################################################
addDateFeatures <- function(cash) {
    
    # add date related features
    cash <- within(cash, {
        atm <- ordered(atm)
        trandate <- as.Date(trandate, format="%m/%d/%Y")
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
    
    return(cash)
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
        holidayN <- as.integer(holiday)
    })
    
    return(cash)
}

addPaydays <- function(cash, paydaysFile, dataDir, forecastTo) {
    
    # pay days - need to collapse multiple pay/pre/post days into one row for each atm/date
    paydays <- read.csv(sprintf("%s/%s", dataDir, paydaysFile))
    paydays$trandate <- as.Date(paydays$date)
    paydays <- subset(paydays, select=c(trandate, payday))
    paydays <- ddply(paydays, "trandate", summarise, payday = paste(payday, collapse="+"))
    
    # ensure that the holidays data set is complete
    if(max(paydays$trandate, na.rm=T) < forecastTo)
        stop(sprintf("missing paydays data up to %s", forecastTo))
    
    # pay days - merge with the cash data
    paydays <- data.table(paydays, key="trandate")
    cash <- merge(x=cash, y=paydays, by="trandate", all.x=TRUE)
    cash$payday[is.na(cash$payday)] <- "none"
    cash <- within(cash, {
        payday <- as.factor(payday)
        paydayN <- as.integer(payday)      
    })
    
    return(cash)
}

addEvents <- function(cash, eventsFile, dataDir) {
    
    # events - clean the data gathered from stub hub
    events <- read.csv(sprintf("%s/%s", dataDir, eventsFile))
    events <- rename(events, c("eventdate"="trandate", 
                               "totalTickets"="eventTickets", 
                               "distance"="eventDistance"))
    events$trandate <- as.Date(events$trandate, format="%m/%d/%Y")
    events <- ddply(events, c("atm","trandate"), 
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
# Adds a mean, min, max, sd when aggregated by the 'by' argument
# to a data frame.
##################################################################
addTrend <- function(data, by, abbrev) {
    loginfo("creating trend by (%s)", by)
    trend <- data[, list(mean=mean(usage, na.rm=T), 
                         min=min(usage, na.rm=T), 
                         max=max(usage, na.rm=T), 
                         sd=sd(usage, na.rm=T)), 
                  by=by]
    
    # ensure that there are no unexpected NAs - should not need this
    trend$mean[is.na(trend$mean)] <- 0
    trend$min[is.na(trend$min)] <- 0
    trend$max[is.na(trend$max)] <- 0
    trend$sd[is.na(trend$sd)] <- 0
    
    # alter the column names - for example day-of-week mean is labelled 'dowMean'
    setnames(trend, c(by, 
                      paste0(abbrev,"Mean"), 
                      paste0(abbrev,"Min"),
                      paste0(abbrev,"Max"), 
                      paste0(abbrev,"Sd")))
    
    loginfo("joining trend with the original data...")
    data <- merge(x=data, y=trend, by=by, all.x=T)
    
    # clean-up the excess data sets to avoid an out-of-memory
    rm(trend)
    gc()
    
    return(data)
}


##################################################################
# Cleans and merges the input data and creates all of the necessary 
# features.  Four input data frames are required; cash, holidays, 
# paydays, and events.  A single 'cash' data frame is returned to be used 
# for training and prediction.
##################################################################
fetch <- function(forecastTo=today()+30,
                  dataDir="../../resources",
                  usageFile="usage-micro.rds",
                  holidaysFile="holidays.csv",
                  eventsFile="events.csv",
                  paydaysFile="paydays.csv") {
    
    cash <- cache("cash", {

        # grab the historical usage data
        history <- readRDS(sprintf("%s/%s", dataDir, usageFile))
        lastDay <- max(history$trandate)
        
        # which 'future' atm-dates need to be forecasted?
        future <- expand.grid(atm=unique(history$atm), 
                              trandate=seq(from=lastDay+1, to=forecastTo, by="days"), 
                              usage=NA_integer_)

        # combine the historical data with what needs forecasted
        cash <- data.table(rbind(history, future), key=c("atm","trandate"))
        
        # build out the feature set with dates, holidays, paydays and events
        cash <- addDateFeatures(cash)
        cash <- addHolidays(cash, holidaysFile, dataDir, forecastTo)
        cash <- addPaydays(cash, paydaysFile, dataDir, forecastTo)
        #cash <- addEvents(cash, eventsFile, dataDir)        
        
        # add trend summaries specific to the ATM
        cash <- addTrend(cash, by=c("atm","weekOfYear"), "woy")
        cash <- addTrend(cash, by=c("atm","monthOfYear"), "moy")
        cash <- addTrend(cash, by=c("atm","dayOfWeek"), "dow")
        cash <- addTrend(cash, by=c("atm","weekOfMonth"), "wom")
        cash <- addTrend(cash, by=c("atm","quarter"), "qua")
        cash <- addTrend(cash, by=c("atm","holidayN"), "hol")
        cash <- addTrend(cash, by=c("atm","paydayN"), "pay")         
        
        # add trend summaries across all of the ATMs
        cash <- addTrend(cash, by="weekOfYear", "woyAll")
        cash <- addTrend(cash, by="monthOfYear", "moyAll")
        cash <- addTrend(cash, by="dayOfWeek", "dowAll")
        cash <- addTrend(cash, by="weekOfMonth", "womAll")
        cash <- addTrend(cash, by="quarter", "quaAll")  
        cash <- addTrend(cash, by="holidayN", "holAll")
        cash <- addTrend(cash, by="paydayN", "payAll") 
        
        # tidy up a bit
        cash <- subset(cash, select=c(8:10,1:7,11:ncol(cash)))
        setkeyv(cash, c("atm", "trandate"))
    })
    
    return(cash)
}
