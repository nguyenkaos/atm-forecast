
##################################################################
# Fetches the ATM usage data and builds a set of features related
# to the date.
##################################################################
fetchUsage <- function(usageFile, libDir) {
    
    # add date related features
    cash <- readRDS(sprintf("%s/%s", libDir, usageFile))
    cash <- data.table(cash, key=c("atm","trandate"))
    cash <- within(cash, {
        usage <- as.integer(usage)
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
addHolidays <- function(cash, holidaysFile, libDir) {
    
    # holidays - clean
    holidays <- read.csv(sprintf("%s/%s", libDir, holidaysFile))
    holidays$holiday <- NULL
    holidays$date <- as.Date(holidays$date, format="%m/%d/%Y")
    holidays <- rename(holidays, c("date"="trandate", "impact"="holiday"))
    holidays <- data.table(holidays, key="trandate")
    
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

addPaydays <- function(cash, paydaysFile, libDir) {
    
    # pay days - need to collapse multiple pay/pre/post days into one row for each atm/date
    paydays <- read.csv(sprintf("%s/%s", libDir, paydaysFile))
    paydays$trandate <- as.Date(paydays$date, format="%m/%d/%Y")
    paydays <- subset(paydays, select=c(trandate, payday))
    paydays <- ddply(paydays, "trandate", summarise, payday = paste(payday, collapse="+"))
    
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

addEvents <- function(cash, eventsFile, libDir) {
    
    # events - clean the data gathered from stub hub
    events <- read.csv(sprintf("%s/%s", libDir, eventsFile))
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
    loginfo("data (input) --> rows = %.0f", nrow(data))
    
    summary <- ddply(data, by, summarise, 
                     mean=mean(usage, na.rm=T), 
                     min=min(usage, na.rm=T), 
                     max=max(usage, na.rm=T), 
                     sd=sd(usage, na.rm=T))
    loginfo("summary --> rows = %.0f", nrow(summary))
    
    # ensure that there are no unexpected NAs
    summary$mean[is.na(summary$mean)] <- 0
    summary$min[is.na(summary$min)] <- 0
    summary$max[is.na(summary$max)] <- 0
    summary$sd[is.na(summary$sd)] <- 0
    
    # alter the column names - for example day-of-week mean is labelled 'dowMean'
    names(summary) <- c(by, 
                        paste0(abbrev,"Mean"), 
                        paste0(abbrev,"Min"),
                        paste0(abbrev,"Max"),
                        paste0(abbrev,"Sd"))
    
    data <- merge(x=data, y=summary, by=by, all.x=T)
    loginfo("data (output) --> rows = %.0f", nrow(data))
    
    # clean-up the excess data sets to avoid an out-of-memory
    rm(summary)
    gc(verbose=T)
    
    return(data)
}


##################################################################
# Cleans and merges the input data and creates all of the necessary 
# features.  Four input data frames are required; cash, holidays, 
# paydays, and events.  A single 'cash' data frame is returned to be used 
# for training and prediction.
##################################################################
fetch <- function(libDir="../../resources",
                  usageFile="usage-micro.rds",
                  holidaysFile="holidays.csv",
                  eventsFile="events.csv",
                  paydaysFile="paydays.csv") {
    
    # cache the results along the way to avoid rework in case of problems
    cash <- cache("cash-no-trends", {
        cash <- fetchUsage(usageFile, libDir)
        cash <- addHolidays(cash, holidaysFile, libDir)
        cash <- addPaydays(cash, paydaysFile, libDir)
        cash <- addEvents(cash, eventsFile, libDir)        
    })
    
    # add trend summaries specific to the ATM
    cash <- cache("cash-w-trends", {
        cash <- addTrend(cash, by=c("atm","weekOfYear"), "woy")
        cash <- addTrend(cash, by=c("atm","monthOfYear"), "moy")
        cash <- addTrend(cash, by=c("atm","dayOfWeek"), "dow")
        cash <- addTrend(cash, by=c("atm","weekOfMonth"), "wom")
        cash <- addTrend(cash, by=c("atm","quarter"), "qua")
        cash <- addTrend(cash, by=c("atm","holidayN"), "hol")
        cash <- addTrend(cash, by=c("atm","paydayN"), "pay")         
    })
 
    # add trend summaries across all of the ATMs
    cash <- cache("cash", {
        cash <- addTrend(cash, by="weekOfYear", "woyAll")
        cash <- addTrend(cash, by="monthOfYear", "moyAll")
        cash <- addTrend(cash, by="dayOfWeek", "dowAll")
        cash <- addTrend(cash, by="weekOfMonth", "womAll")
        cash <- addTrend(cash, by="quarter", "quaAll")  
        cash <- addTrend(cash, by="holidayN", "holAll")
        cash <- addTrend(cash, by="paydayN", "payAll")        
    })
    
    return(cash)
}
