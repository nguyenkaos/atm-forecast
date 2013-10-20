
library("logging")
library("data.table")
library("plyr")

source("utils.R")
source("../common/cache.R")
source("../forecast/fetch.R")
source("../forecast/train.R")
source("../forecast/score.R")

cluster.random <- function (cluster.dir = "micro", 
                            history.file = "deposits-micro.rds") {
    
    features.cache <- sprintf("%s-features", basename.only(history.file))
    features <- cache (features.cache, {
        
        # fetch the usage history and generate the features for training
        fetch( history.file = history.file,
               forecast.to  = today(),
               data.dir     = "../../resources")
    })
    
    atms <- unique(features$atm)
    atms.count <- length(atms)
    
    # randomly choose the number of clusters from each quantile
    attempts.per.quant <- 1
    n.clusters <- sapply (2:4, function(q) {
        floor (sample (quant[q-1]:quant[q], attempts.per.quant))
    })
    
    # assign each ATM to a cluster
    assignments <- data.table (expand.grid (cluster.set = n.clusters, atm = atms))
    assignments[, cluster := sample (1:n, 1), by = list(cluster.set, atm)]
    
    # output each cluster set
    d_ply(assignments, "cluster.set", function (cluster.defn) {
        csv.name <- sprintf ("%s/random-%s.csv", cluster.dir, unique(cluster.defn$cluster.set))
        csv.data <- subset (cluster.defn, select = c("atm", "cluster"))
        write.csv (csv.data, csv.name, row.names = FALSE)
    })
    
    
    
    
    
}