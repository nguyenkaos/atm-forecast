library("plyr")
library("dtw")
library("fpc")
library("ggplot2")
library("reshape2")
library("gdata")
library("caret")

source("utils/cache.R")

############################################################################
# clean and preprocess the input data
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

############################################################################
# transform the cash usage for each ATM into a time series
usage <- cache("usage", {
    dlply(withd, "atm", function(byAtm) as.vector(byAtm$usage), .progress="text")
})

############################################################################
# create a 'tree' using hclust based on each ATM's cash usage time series.
# that can be used to create any number of clusters and is rather expensive to build.
tree <- cache("tree", {
    hclust(dist(usage, method="DTW"), method="ward")  
})


# TODO - iterate through numberOfClusers from 1 to 8705
# TODO - generage the clusters
# TODO - in each cluster, set aside a few hold-out ATMs
# TODO - calculate an "average" cash usage for the cluster
# TODO - (a) forecast the hold-out ATMs based on their actual usage
# TODO - (b) forecast the hold-out ATMs based on the cluster average usage
# TODO - the best cluster size is the one with minimum of (a) - (b)


############################################################################
# cluster the ATMs based on their cash usage time series
clusters <- cache("clusters", {
    numberOfClusters = 6
    
    clusters <- cutree(tree, k=numberOfClusters) 
    clusters <- within(clusters, {
        cluster <- sapply(atm, function(atm) as.integer(clusters[as.character(atm)]))
        cluster <- factor(cluster, levels=c(1:numberOfClusters))       
    })
})






