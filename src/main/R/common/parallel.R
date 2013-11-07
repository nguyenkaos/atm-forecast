#
# This file should be sourced to enable some level of parallelism, especially
# when using Plyr and Caret.
#

# parallel backend for UNIX
library("doMC")
registerDoMC()

# parallel backend for Windows
#library("parallel")
#cl <- makeCluster(detectCores())
#stopCluster(cl)