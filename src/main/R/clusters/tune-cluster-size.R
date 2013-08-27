
source("gbm.R")
source("clusters.R")

clusters.clean()
clusters.transform()


numberOfClusters = 10
atmsByCluster <- clusters.cluster(numberOfClusters)

# TODO - still need to 
july <- gbm.forecast(cash, by="cluster")
print(sum(july$totalScore))

# TRAIN BY CLUSTER INSTEAD OF TRAIN BY ATM???
# TODO on test set - forecast by cluster vs forecast by ATM history alone