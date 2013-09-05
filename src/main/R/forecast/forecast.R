
##################################################################
# Creates the competition forecast.  Returns the total
# competition score for all dates between 'start' and 'end'.
##################################################################
forecast <- function(cash, start, end, by="atm", parallel=F) {
    
    # train and score the model by atm
    scoreByAtm <- ddply(cash, by, trainAndScore, .parallel=parallel)
    saveRDS(scoreByAtm, "scoreByAtm.rds")  
    
    # summarize the scores by day
    scoreByDate <- ddply(scoreByAtm, ~trandate, summarise, totalScore=sum(score), .parallel=parallel)
    saveRDS(scoreByDate, "scoreByDate.rds")
    
    # grab the scored forecasts within the defined time period
    scored <- subset(scoreByDate, trandate>=as.Date(start) & trandate<=as.Date(end))
    
    return(scored)
}
