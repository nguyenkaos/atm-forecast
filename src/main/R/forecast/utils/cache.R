
# TODO - use 'log4R' package instead of 'print'

############################################################################
# Executes a function 'fn' and caches the result under the name 'cache'.
# If the result has already been cached, it will be returned from the 
# cache instead of re-calculated.  This can save significant time for 
# functions that don't need to always be re-run.
############################################################################
cache <- function(cache, fn, ..., cacheDir="work") {
    result <- NULL
    
    # create the working directory, if necessary
    dir.create(path=cacheDir, showWarnings=FALSE)
    
    cacheFile <- sprintf("%s/%s.rds", cacheDir, cache)
    if(file.exists(cacheFile)) {
        # the result has already been cached
        print(sprintf("the result has already been cached: %s", cacheFile))
        result <- readRDS(cacheFile)
        
    } else {
        # calculate the result and cache it
        print(sprintf("the result has NOT been cached: %s", cacheFile))
        result <- fn(...)
        saveRDS(result, cacheFile)
    }
    
    return(result)
}
