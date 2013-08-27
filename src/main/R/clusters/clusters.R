library("plyr")
library("dtw")
library("fpc")
library("ggplot2")
library("reshape2")
library("gdata")
library("caret")
library("fastcluster")  # overwrites stats::hclust 
library("logging")

source("../common/cache.R")

# setup logging
basicConfig(level=20)

############################################################################
# clean and preprocess the input data
clusters.clean <- function() {
  atms <- cache("atms", {
    
    # load the geo, profile and withdrawals data
    withd <- readRDS("../../resources/withdrawals.rds")
    geo <- readRDS("../../resources/geocore.rds")
    profiles <- readRDS("../../resources/profiles.rds")    
    
    # stardize the messy feature names
    names(profiles) <- make.names(tolower(names(profiles)), allow_=F)
    names(profiles)[names(profiles) == 'terminal.id'] <- 'atm'
    
    # merge the geo and profiles data
    atms <- merge(profiles, geo, by="atm")
    
    # filter out any ATMs for which there is no cash usage data
    atms <- subset(atms, atm %in% unique(withd$atm))
  })
}

############################################################################
# transform the cash usage for each ATM into a time series
clusters.transform <- function() {
  usage <- cache("usage", {
    
    # load the geo, profile and withdrawals data
    withd <- readRDS("../../resources/withdrawals.rds")
    usage <- dlply(withd, "atm", function(byAtm) rev(as.vector(byAtm$usage)), .progress="text")
    
    # ATMs will all have time series of different lengths; need most recent data 
    # first to allow ATMs with different life spans to be compared
    usage <- data.frame(atm=names(usage), time_series=I(usage))
    rownames(usage) <- NULL
    usage
  })
}

clusters.cluster <- function(numberOfClusters) {
  
  ############################################################################
  # create a 'tree' using hclust based on each ATM's cash usage time series.
  # that can be used to create any number of clusters and is rather expensive to build.
  tree <- cache("tree", {
    hclust(dist(usage$time_series, method="DTW"))  
  })
  
  ############################################################################
  # cluster the ATMs based on their cash usage time series
  cAtms <- cache("cAtms", {
    numberOfClusters = 10
    cAtms <- atms
    rownames(cAtms) <- NULL
    
    # cluster the ATMs
    clusters <- cutree(tree, k=numberOfClusters) 
    cAtms <- within(cAtms, {
      cluster <- sapply(atm, function(atm) as.integer(clusters[as.character(atm)]))
      cluster <- factor(cluster, levels=c(1:numberOfClusters))       
    })
    
    # mark the training vs test ATMs
    cAtms$isTrain <- sapply(rownames(cAtms), function(row) { 
      row %in% createDataPartition(cAtms$cluster, p=0.8, list=F)
    })
    
    cAtms  # ensures that it is cached
  })
  
  return(cAtms)
}

############################################################################
# Create a model to predict the clusters based on the Profile/Geo data alone.
clusters.predict <- function() {
  fit <- cache("fit", {
    
    # split into test and training sets
    train <- subset(cAtms, isTrain==T)
    test <- subset(cAtms, isTrain==F)
    
    # remove features that cannot be used or will not be available for new ATMs
    predictors <- subset(train, 
                         select=-c(isTrain, name, location.desc, address, city, 
                                   zipcode, conversion.date, atm.install.date, int.zip, 
                                   branch.latitude, branch.longitude, combinedservice,
                                   adj.ews, cbsa.id, withdrawals.past.2wk, billcount.past.2wk, 
                                   frequency, rep.per.wk ))
    outcome <- train$cluster
    
    # fit a model
    fit <- train(x=predictors, y=outcome, method="gbm", verbose=T,
                 trControl=trainControl(method="repeatedcv", number=5, repeats=5))
    
    ######################################################################
    # PROBLEM/TODO
    # Using the {x,y} interface for train is much faster, but for the most
    # part always predicts cluster 3.  Huh?
    # 
    # Now trying with formula interface which is waaay slower, but want to
    # see the predictions.
    ######################################################################
  })
}

############################################################################
# Predict the clusters based on the Profile, Geo data alone.
clusters.score <- function() {
  predictions <- cache("predictions", {
    
    # for some reason leaving the 'cluster' field causes a cluster-f***
    test <- subset(cAtms, !isTrain)
    testCluster <- test$cluster
    test$cluster <- NULL
    
    # use the model to predict the clusters
    test$clusterHat <- predict.train(fit, newdata=test)
    test$cluster <- testCluster
    
    # for some reason leaving the 'cluster' field causes a cluster-f***
    train <- subset(cAtms, isTrain)
    trainCluster <- train$cluster
    train$cluster <- NULL
    
    # use the model to predict the clusters - even for the training set
    train$clusterHat <- predict(fit, newdata=train)
    train$cluster <- trainCluster
    
    predictions <- rbind.fill(train, test)
  })
}



