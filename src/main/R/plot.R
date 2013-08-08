library("ggplot2")
library("plyr")
library("reshape")
library("caret")

cash <- readRDS("work/cash.rds")

# overall feature importance
scores <- readRDS("work/scoreByAtm.rds")
byAtm <- subset(scores, select=c(1,21:30))
byAtm <- byAtm[!duplicated(byAtm),]

# remove the expected low scorers
byAtm <- subset(byAtm, select=-c(impPaydayN, impEventDistance, impHolidayN, impWeekOfYear))
byAtm <- melt(byAtm, id.vars=c("atm"))

cdf <- ddply(byAtm, .(variable), summarise, mean=mean(value))
cdf <- cdf[order(cdf$mean, decreasing=T),]

# helps order the plot by importance
# byAtm$variableO <- factor(byAtm$variable, levels=cdf$variable)
# byAtm$mean <- sapply(byAtm$variable, function(var) { 
#    return(cdf[cdf$variable==var, "feature.mean"]) 
#})
 
ggplot(byAtm, aes(x=value, fill=variable)) +
    geom_density(alpha=.3) +
    xlab("Relative Importance") +
    ylab("Density") +
    scale_fill_discrete(guide=FALSE) +
    geom_vline(data=cdf, aes(xintercept=mean, color=variable), linetype="dashed", size=0.75) +
    facet_wrap(.~variable) 
    #facet_grid(variable~., labeller=function(variable, value) { return(gsub("imp","",value)) }) 
    

# take a closer look at the other features
byAtm <- subset(scores, select=c(1,21:30))
byAtm <- byAtm[!duplicated(byAtm),]
byAtm <- melt(byAtm, id.vars=c("atm"))

# how many models used each?  may not show itself since for each ATM few holidays and paydays
subset(byAtm, variable=='impEventDistance' & value>0)
subset(byAtm, variable=='impPaydayN' & value>0)
subset(byAtm, variable=='impHolidayN' & value>0)
subset(byAtm, variable=='impWeekOfYear' & value>0)

# where was the model successful?? where was it not??
byAtm <- readRDS("../results/gbmScoreByAtm.rds")
cash <- readRDS("work/cash.rds")
cash$usage <- NULL
scores <- merge(byAtm,cash,by=c('atm','trandate'))

# clean it
scores$mapeC <- sapply(scores$mape, function(mape) if(mape > 2) return(2) else return(mape) )

bucketByMape <- function(mape) {
    bucket <- NULL
    if(mape < 0.05)
        bucket <- "<5%"
    else if(mape < 0.10)
        bucket <- "<10%"
    else if(mape < 0.20)
        bucket <- "<20%"
    else if(mape < 0.50)
        bucket <- "<50%"
    else
        bucket <- "50%+"
    return(bucket)
}
scores$mapeB <- sapply(scores$mape, bucketByMape)
scores$mapeB <- factor(scores$mapeB)


# plot the continuous features
#featurePlot(x=scores[,c("usage","trandateN","dayOfWeek","dayOfYear","dayOfQuarter","dayOfSemiYear","weekOfMonth","weekOfYear","eventTickets","eventDistance")], 
#            y=scores$mapeC,
#            plot="scatter", layout=c(3,3), span=0.5, type=c("p","smooth"))


ggplot(byAtm, aes(x=mape, fill=weekOfYear)) +
    geom_density(alpha=.3) +
    xlab("Relative Importance") +
    ylab("Density") +
    scale_fill_discrete(guide=FALSE) +
    geom_vline(data=cdf, aes(xintercept=mean, color=variable), linetype="dashed", size=0.75) +
    facet_wrap(.~) 

