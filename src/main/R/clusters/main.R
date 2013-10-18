#!/usr/bin/env Rscript

library("optparse")
library("lubridate")

source("score.R")

basicConfig(level=loglevels["INFO"])

# defines the options/arguments
getOptions <- function() {
    all_options <- list(
        make_option(c("--clusterDir"),
                    help    = "Directory containing the cluster definitions [default: %default]",
                    default = "micro"),
        make_option(c("--historyFile"),
                    help    = "File containing the usage history [default: %default]",
                    default = "deposits-micro.rds")
    )
    opts <- parse_args (OptionParser (option_list = all_options))
}

# gather the command line options
opts <- getOptions()
scores <- scoreAllClusters(opts$clusterDir, opts$historyFile)

# save the scores
scores.file <- sprintf("cluster-scores-%s.csv", today())
write.csv(scores, scores.file, row.names=F)
loginfo("created %s", scores.file)

# output the scores
scores


