############################################################################
# This file should be sourced to enable some level of parallelism, especially
# when using Plyr.
############################################################################

library("doMC")
registerDoMC(cores = 8)

