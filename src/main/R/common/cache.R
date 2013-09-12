
############################################################################ 
# Executes an expression 'expr' and caches the result under the name 
# 'cacheName'. If the expression result has already been cached, it will be 
# returned from the cache instead of re-calculated.  This can save significant 
# time for functions that don't need to always be re-run. 
############################################################################
cache <- function(cacheName, expr, cacheDir=".cache2", clearCache=F) {
    result <- NULL
    cacheFile <- sprintf("%s/%s.rds", cacheDir, cacheName)
    
    # has the result already been cached?
    if(file.exists(cacheFile) && clearCache==F) {
        loginfo("Found '%s' cached as '%s'", cacheName, cacheFile)
        result <- readRDS(cacheFile)
        
    } else {
        # eval the expression 
        loginfo("'%s' has NOT been cached", cacheName)
        result <- eval(expr)

        # sanity check, just in case
        if(0 == length(result))
            warning("attempting to cache an empty result", immediate.=T)
        
        # create the cache directory, if necessary
        dir.create(path=cacheDir, showWarnings=FALSE)
        
        # cache the result
        loginfo("Cacheing '%s' as '%s'", cacheName, cacheFile)
        saveRDS(result, cacheFile)
    }
    
    return(result)
}

