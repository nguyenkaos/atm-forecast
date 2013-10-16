
library("logging")
library("data.table")
library("plyr")

source("utils.R")
source("../forecast/fetch.R")
source("../forecast/train.R")
source("../forecast/score.R")

basicConfig(level=loglevels["INFO"])

#
# Scores all of the cluster definitions within a directory.
#
scoreAllClusters <- function (cluster.dir = "micro", 
                              history.file = "deposits-micro.rds") {
    
    cluster.files <- list.files(cluster.dir, full.names=T)
    scores <- ldply(cluster.files, function(cluster.path) { 
        forecastByCluster(cluster.path, history.file)
    })
}

#
# Creates a forecast by training within a defined set of clusters.
#
forecastByCluster <- function (cluster.path = "micro/random-clusters.csv", 
                               history.file = "deposits-micro.rds") {
    
    # create a name/id for this cluster definition
    cluster.set.id <- basename.only(cluster.path)
    loginfo("training with cluster: '%s'", cluster.set.id)
    
    # create a unique name for the deposit feature set to allow it to be cached easily
    deposits.cache.name <- sprintf("%s-features", basename.only(history.file))
    
    # read in the feature set
    deposits <- cache (deposits.cache.name, {
        fetch( history.file = history.file,
               forecast.to  = today() + 30,
               data.dir     = "../../resources")
    })
    
    # fetch the cluster definition
    loginfo("fetching and merging the clusters defined by %s", cluster.path)
    csv <- read.csv(cluster.path, col.names = c("atm","cluster"))
    clusters <- data.table(csv, key="atm")
    
    # merge the feature set with the cluster set
    deposits[clusters, cluster := cluster]
    deposits[,`:=`(
        atm = factor(atm),
        atm = as.integer(atm)
    ),]
    
    # forecast by cluster - only train those with a cluster
    score.by.atm <- deposits[
        !is.na(usage) & !is.na(cluster), 
        c("usage.hat","ape","score") := trainAndScore(
            .BY, 
            .SD, 
            method       = "gbm",
            split.at     = "2013-06-30",
            default      = expand.grid(.interaction.depth=2, .n.trees=50, .shrinkage=0.1), 
            verbose      = F, 
            distribution = "poisson",
            cache.prefix = sprintf("%s-fit", cluster.set.id)),
        by=cluster]
    
    # show the scores for july and august
    score.by.atm [
        trandate > '2013-06-30' & trandate < '2013-09-01',
        list(
            cluster.set = cluster.set.id,
            score       = sum(score, na.rm=T),
            mape        = mean(ape,na.rm=T),
            count       = length(unique(atm))
        ), by=month(trandate)]
}
