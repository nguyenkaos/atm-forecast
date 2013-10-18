#!/usr/bin/env Rscript

library("logging")
library("data.table")
library("plyr")

source("utils.R")
source("../common/cache.R")
source("../forecast/fetch.R")
source("../forecast/train.R")
source("../forecast/score.R")

#
# Scores all of the cluster definitions within a directory.
#
scoreAllClusters <- function (cluster.dir = "micro", 
                              history.file = "deposits-micro.rds") {
    
    # create a forecast for each cluster definition
    scores <- cache (sprintf("scores-%s", cluster.dir), {
        
        # find all the cluster definition files
        cluster.files <- list.files(cluster.dir, full.names=T)
        
        # aggregate the forecast score for each cluster definition
        scores <- ldply(cluster.files, function(cluster.path) { 
            forecastByCluster(cluster.path, history.file)
        })
        
        data.table(scores, key = c("atm","trandate"))
    })
    
    # extract the baseline/champion scores (no clusters)
    champ.name <- "no-clusters"
    champ <- scores [cluster.set == champ.name & is.finite(usage)]
    setnames(champ, "ape", "ape.champ")
    setnames(champ, "score", "score.champ")
    
    # add the champ's scores to the competitors data for comparison
    competitors <- scores [is.finite(usage) & is.finite(usage.hat)]
    competitors [champ, `:=`(
        ape.delta = ape - ape.champ,
        score.delta = score - score.champ
    )]
    
    # add in the 9 volatility/volume segments
    atm.segments <- data.table (read.csv ("../../resources/atm-segments.csv"), key = "atm")
    
    # the NA for volatility/volume is coming from ATM 'OR2430'
    competitors [atm.segments, `:=` (
        volatility = volatility.segment,
        volume = volume.segment
    )]

    scores.final <- competitors [
        is.finite(volatility) & is.finite(volume),
        list(
            mape.delta = mean(ape.delta, na.rm=T),
            score.delta = sum(score.delta, na.rm=T)
        ), 
        by = list(cluster.set, volatility, volume)]
}

#
# Creates a forecast by training within a defined set of clusters.
#
forecastByCluster <- function (cluster.path = "micro/random-clusters.csv", 
                               history.file = "deposits-micro.rds") {
    
    # create a uniqe name for this cluster definition
    cluster.set.id <- basename.only(cluster.path)
    loginfo("training with cluster: '%s'", cluster.set.id)
    
    # create a unique name for the deposit feature set to allow it to be cached easily
    deposits.cache.name <- sprintf("%s-features", basename.only(history.file))
    
    # read in the feature set
    deposits <- cache (deposits.cache.name, {
        fetch( history.file = history.file,
               forecast.to  = today(),
               data.dir     = "../../resources")
    })
    
    # fetch the cluster definition
    loginfo("fetching and merging the clusters defined by %s", cluster.path)
    csv <- read.csv(cluster.path, col.names = c("atm", "cluster"))
    clusters <- data.table(csv, key="atm")
    
    # merge the feature set with the cluster definition
    deposits[clusters, cluster := cluster]
    
    # forecast by cluster - only train those with a cluster
    score.by.atm <- deposits[
        
        # only train those that are part of a cluster
        !is.na(cluster) & !is.na(usage), 
        
        # defines how the training occurs
        c("usage.hat","ape","score") := trainAndScore (
            .BY, 
            .SD, 
            method       = "gbm",
            split.at     = "2013-06-30",
            formula      = usage ~ . -atm,
            default      = expand.grid (.interaction.depth = 2, 
                                        .n.trees = 50, 
                                        .shrinkage = 0.1),
            cache.prefix = sprintf("%s-fit", cluster.set.id),
            
            # parameters specific to the training method
            verbose      = F, 
            distribution = "poisson",
            keep.data    = FALSE),
        
        # trained separately for each cluster
        by = cluster ]
    
    # add in the name of the cluster set that was forecasted
    score.by.atm[, cluster.set := cluster.set.id, ]
}
