
library("plyr")
library("dtw")
library("fpc")
library("ggplot2")
library("reshape2")
library("gdata")
library("caret")

numberOfClusters = 6

# create a working directory
dir.create(path="./work", showWarnings=FALSE)

############################################################################
# Cluster the ATMs based on their cash usage time series.
############################################################################
pathToAtms <- "work/atms.rds"
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
    groups <- cutree(tree, k=numberOfClusters) 
    atms$group <- sapply(atms$atm, function(atm) as.integer(groups[as.character(atm)]))
    atms$group <- factor(atms$group, levels=c(1:numberOfClusters))
    
    saveRDS(atms, pathToAtms)
    keep(atms, sure=T)
}

############################################################################
# Predict the clusters based on the Profile, Geo data alone.
############################################################################

# split into test and training sets
ind <- createDataPartition(atms$group, p=0.8, list=F)
test <- atms[-ind,]
train <- atms[ind,]

# instead of using a formula, better performance with many predictors
# gbm does not currently handle categorical variables with more than 1024 levels or dates
predictors <- subset(train, select=-c(atm, group, NAME, LOCATION_DESC, Address, City, 
                                      ZipCode, conversion_date, ATM_INSTALL_DATE, INT_ZIP))
outcomes <- subset(train, select=group, drop=T)

# fit a model 
fit <- train(x=predictors, y=outcomes, method="gbm", distribution="multinomial")
saveRDS(fit, "work/cluster-fit.rds")

# use the model to predict the group
test$groupHat <- predict(fit, newdata=test)
train$groupHat <- predict(fit, newdata=train)

# save the results
test$isTest <- T 
train$isTest <- F
clusters <- rbind.fill(train, test)

clusters$groupHat <- round(clusters$groupHat)
clusters$groupHat <- factor(clusters$groupHat, levels=c(1:numberOfClusters))
clusters$group <- factor(clusters$group, levels=c(1:numberOfClusters))

saveRDS(clusters, "work/clusters.rds")  

keep(clusters, fit)


