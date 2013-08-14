
# TODO - use 'log4R' package instead of 'print'

############################################################################ 
# Executes an expression 'expr' and caches the result under the name 
# 'cacheName'. If the expression result has already been cached, it will be 
# returned from the cache instead of re-calculated.  This can save significant 
# time for functions that don't need to always be re-run. 
############################################################################
cache <- function(cacheName, expr, cacheDir="work", clearCache=F) {
    result <- NULL
    
    # create the cache directory, if necessary
    dir.create(path=cacheDir, showWarnings=FALSE)
    cacheFile <- sprintf("%s/%s.rds", cacheDir, cacheName)
    
    # has the result already been cached?
    if(file.exists(cacheFile) && clearCache==F) {
        print(sprintf("the result has already been cached: %s", cacheFile))
        result <- readRDS(cacheFile)
        
        # eval the expression and cache its result
    } else {
        print(sprintf("the result has NOT been cached: %s", cacheFile))
        result <- eval(expr)
        saveRDS(result, cacheFile)
    }
    
    return(result)
}

