
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
    
    features.cache <- sprintf("%s-features", basename.only(history.file))
    features <- cache (features.cache, {
        
        # fetch the usage history and generate the features for training
        fetch( history.file = history.file,
               forecast.to  = today(),
               data.dir     = "../../resources")
    })
    
    scores.cache <- sprintf ("scores-%s", cluster.dir)
    scores <- cache (scores.cache, {
        
        # create data set with all cluster definitions
        scores <- data.table (cluster.file = list.files (cluster.dir, full.names=T))
        
        # create a forecast for each cluster definition
        scores <- scores[ , forecastByCluster (features, .BY), by = cluster.file]
        setkeyv (scores, c("atm", "trandate"))
    })
    
    # extract the baseline/champion scores (no clusters)
    champ.name <- "no-clusters"
    champ <- scores [cluster.set == champ.name & is.finite(usage)]
    setnames(champ, "ape", "ape.champ")
    setnames(champ, "score", "score.champ")
    
    # add the champ's scores to the competitors data for comparison
    competitors <- scores [is.finite(usage) & is.finite(usage.hat)]
    competitors [champ, `:=`(
        ape.delta   = ape - ape.champ,
        score.delta = score - score.champ
    )]
    
    # add in the 9 volatility/volume segments
    atm.segments <- data.table (read.csv ("../../resources/atm-segments.csv"), key = "atm")
    
    # the NA for volatility/volume is coming from ATM 'OR2430'
    competitors [atm.segments, `:=` (
        volume     = volume.segment,
        volatility = volatility.segment
    )]
    
    # aggregate the scores
    scores.final <- competitors [
        is.finite(volatility) & is.finite(volume),
        list(
            mape        = mean(ape),
            mape.delta  = mean(ape.delta),
            score       = sum(score),
            score.delta = sum(score.delta)
        ), 
        by = list(cluster.set, volatility, volume)]
}

#
# Creates a forecast by training within a defined set of clusters.
#
forecastByCluster <- function (features, cluster.path = "micro/random-clusters.csv") {
    if(!is.character(cluster.path))
        cluster.path <- cluster.path[[1]]
    
    # create a uniqe name for this cluster definition
    cluster.set.id <- basename.only (cluster.path)
    loginfo ("training with cluster: '%s'", cluster.set.id)
    
    # fetch the cluster definition
    loginfo ("fetching and merging the clusters defined by %s", cluster.path)
    csv <- read.csv (cluster.path, col.names = c("atm", "cluster"))
    clusters <- data.table (csv, key="atm")
    
    # merge the feature set with the cluster definition
    features[clusters, cluster := cluster]
    
    # forecast by cluster - only train those with a cluster
    score.by.atm <- features[
        
        # only train those that are part of a cluster
        !is.na(cluster) & !is.na(usage), 
        
        # defines how the training occurs
        c("usage.hat", "pe", "ape", "score") := trainAndScore (
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
    
    # return the raw scoring results for this cluster
    with (score.by.atm, list (atm         = atm, 
                              trandate    = trandate, 
                              usage       = usage, 
                              usage.hat   = usage.hat, 
                              ape         = ape, 
                              score       = score,
                              cluster.set = cluster.set.id))
}
