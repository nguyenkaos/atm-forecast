
library("plyr")
library("dtw")
library("fpc")
library("ggplot2")
library("reshape2")
library("gdata")
library("caret")

# cluster the ATMs based on cash usage
pathToAtms <- "../work/atms.rds"
if(file.exists(pathToAtms)) {
    print(sprintf("loading data from '%s'", pathToAtms))
    atms <- readRDS(pathToAtms)
    
} else {
    print(sprintf("calculating ATM data which will be saved here: '%s'", pathToAtms))
    
    # load/merge the geo, profile and withdrawals data
    geo <- readRDS("../../resources/geocore.rds")
    profiles <- readRDS("../../resources/profiles.rds")
    names(profiles)[names(profiles) == 'terminal_id'] <- 'atm'
    atms <- merge(profiles, geo, by="atm")
    withd <- readRDS("../../resources/withdrawals.rds")
    
    # take a sample of the data only
    atmSample <- sample(unique(withd$atm), 87)
    atms <- subset(atms, atm %in% atmSample)
    withd <- subset(withd, atm %in% atmSample)
    
    # transform the data for each ATM into a time series
    usage <- dlply(withd, "atm", function(byAtm) as.vector(byAtm$usage), .progress="text")
    
    # calculate the dist between each ATM and build a tree
    tree <- hclust(dist(usage, method="DTW"), method="ward")   
    
    # cluster the ATMs based on their cash usage time series
    groups <- cutree(tree, k=6) 
    atms$group <- sapply(atms$atm, function(atm) as.integer(groups[as.character(atm)]))
    
    saveRDS(atms, pathToAtms)
    keep(atms, sure=T)
}

# predict the group based on the profile/geo data alone
train <- function(data) {
    if(nrow(data)>1) {
        print(sprintf("training ATM '%s' \n", unique(as.character(data$atm))))
    
        # PROBLEM - THERE IS ALWAYS 1 ROW FOR EACH ATM!  HOW CAN THIS BE TRAINED??
        # split the data
        ind <- createDataPartition(data$group, p=0.8, list=F, times=1)
        train <- data[ind,]
        train$isTest <- F
        
        test <- data[-ind,]
        test$isTest <- T 
        
        # fit a model 
        fit <- NULL
        tryCatch(
            fit <- train(form=group~., data=train, method="gbm", 
                         trControl=trainControl(method="repeatedcv", number=5, repeats=5), 
                         verbose=F, distribution="poisson"),
            error = function(e) {
                print(e) 
            }
        )
        
        # use the model to predict the group
        data$groupHat <- predict(fit, newdata=data)
        
    } else {
        print(sprintf("not able to train for ATM '%s' \n", unique(as.character(data$atm))))
    }
    
    return(data)
}

# train and score the model by atm
clusters <- ddply(atms, "atm", train, .parallel=FALSE, .progress="text")
saveRDS(clusters, "../work/cluster.rds")  




# ?how to measure the quality of the clusters? 
#   - a forecast using cluster average history is "close" to forecast with own history
#   - take through whole process - forecast a new ATM based on average of its cluster and review accuracy... just
#           average each day's usage column wise

# ?center and scale the time series before clustering?

# ?cluster over only recent history?
# plot the time series for comparison
#withd <- subset(withd, trandate>as.Date('2013-02-28','%Y-%m-%d'))
#withd <- subset(withd, atm %in% names(atms))
#withd$group <- sapply(withd$atm, function(a) as.integer(groups[as.character(a)]))
#ithd <- melt(withd, id=c("atm","trandate","group"))
#ggplot(withd, aes(x=trandate, y=value, linestyle=group, colour=group, group=atm)) + geom_line()


# review the model's success 
#featurePlot(x=test[,-c("group")], y=test$group, plot="pairs", auto.keys=list(columns=6))


