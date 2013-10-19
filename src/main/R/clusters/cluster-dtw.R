#!/usr/bin/env Rscript

library("data.table")
library("dtw")
library("logging")
library("plyr")

source("../forecast/fetch.R")
source("utils.R")

basicConfig(level=loglevels["INFO"])

# configuration
number.of.clusters <- 20
path <- "../../resources/deposits-mini.rds"

loginfo("convert history (either deposits or withdrawals) to a time series")
atms <- as.ts (fetchHistory (history.path = path, forecast.to = as.Date("2013-08-31")))

loginfo("create a 'tree' using hclust based on each ATM's cash usage time series")
tree.cache.name <- sprintf("dtw-tree-w-%s", basename.only(path))
tree <- cache (tree.cache.name, {
    hclust( dist( atms$time.series, method="DTW"))  
})

loginfo("create clusters from the 'tree'")
atms[, `:=`(
    clusters = cutree(tree, k = number.of.clusters),
    time.series = NULL
), ]

loginfo("export the clusters")
csv <- sprintf("dtw-clusters-w-%s", basename.only(path))
write.csv(atms, "dtw-clusters.csv", row.names=F)

