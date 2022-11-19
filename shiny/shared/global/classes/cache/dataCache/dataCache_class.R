#----------------------------------------------------------------------
# object class for managing data objects stored on disk and/or cached
# in memory during use; not unlike storr, but more specific to our needs
#----------------------------------------------------------------------
# when requested, disk caching is sent to CACHE_DIR/parentType
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN CLASS
#----------------------------------------------------------------------
new_dataCache <- function(parentType) {
    class <- 'dataCache' 
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize class
#----------------------------------------------------------------------
parentDir <- file.path(serverEnv$CACHE_DIR, parentType)
if(!dir.exists(parentDir)) dir.create(parentDir)
cache <- list()
createdOnce <- list() # logical by cache key whether we've seen this object yet this session
sep <- "__"
getCacheKeys <- function(type, keyObject = NULL, key = NULL){
    if(!is.null(keyObject)) key <- digest(keyObject) # hash the keyObject
    if(is.null(key)) return(NULL)
    list(
        key = key, # digest of keyObject, or the user provided key; can be shared by multiple cached objects
        cacheKey = paste(key, type, sep = sep) # the key for cached storage == key__type
    )                                          # unique for a single cached object
}
splitCacheKey <- function(cacheKey) {
    x <- as.list(strsplit(cacheKey, sep)[[1]])
    names(x) <- c('key', 'type')
    x
}

#----------------------------------------------------------------------
# create systematic names for directories and files based on MD5 digest of keyObject
#----------------------------------------------------------------------
getObjectFile <- function(cacheKey, create = FALSE){
    x <- splitCacheKey(cacheKey)
    dir <- getKeyedDir(parentDir, x$key, create = create)    
    if(create && !dir.exists(dir)) dir.create(dir)
    paste0(dir, '/', x$type, '.rds') 
}

#----------------------------------------------------------------------
# create systematic structure for cached objects
#----------------------------------------------------------------------
getStructuredObject <- function(keyObject, keys, value, permanent){
    if(is.null(value)) return(NULL)
    list(
        cacheKey  = keys$cacheKey,  # return the cacheKey to allow caller to use in calls to set and clear; == key__type
        keyObject = keyObject,      # return the keyObject so caller can easily re-learn about the data in value
        key       = keys$key,       # digest of keyObject == cacheKey without the type; for matching keys between cached objects # nolint
        value     = value,          # the data payload
        timestamp = if(permanent) Sys.time() else NULL # a timestamp for comparing default and cached values
    ) 
}
    
#----------------------------------------------------------------------
# single function to handle getting, setting, caching and disk copy of objects
#----------------------------------------------------------------------
get <- function(
    type,             # a human readable name for the kind of object this is within its parentType
    keyObject = NULL, # either keyObject or key is required
    key = NULL,       # should encompass all parameters that define the object's contents
    #----------------------------------------------------------------------
    permanent = TRUE, # permanent means we copy to disk; permanent==FALSE is incompatible with from=='disk'
    from = c('ram', 'disk'), # from determines where we are allowed to get data; disk disables the RAM cache
    #----------------------------------------------------------------------
    create = c('asNeeded', 'once', 'always'), # create determines when we are obliged to call createFn
    createFn = NULL, # for missing/potentially stale objects, we call createFn to create them anew
    ...  # optional named additional arguments passed to createFn, along with cacheKey, keyObject, key, cacheObject
    #----------------------------------------------------------------------
){
    # a unique identifier for this object instance of 'parentType' + 'type', suitable for nested path names
    keys <- getCacheKeys(type, keyObject, key) 
    if(is.null(keys)) return(NULL)
    
    # force creation if requested
    forceCreate <- ( create[1] == 'once' && is.null(createdOnce[[keys$cacheKey]]) ) ||
                     create[1] == "always"
    if(forceCreate) cache[[keys$cacheKey]] <<- NULL          
    
    # recover or create the return value
    cacheObject <- cache[[keys$cacheKey]] # ephemeral object used and garbage collected if from=='disk'
    if(is.null(cacheObject)){
        
        # first from permanent disk copy, if it exists ...
        if(permanent){ 
            file <- getObjectFile(keys$cacheKey, create = FALSE)
            if(file.exists(file)) cacheObject <- readRDS(file)
        }

        # ... otherwise by letting caller create it anew;
        # can also call createFn to check the validity/freshness of disk cache based on create option
        if((forceCreate || is.null(cacheObject)) && !is.null(createFn)) { 
            value <- createFn(cacheKey = keys$cacheKey, keyObject = keyObject, 
                              key = keys$key, cacheObject = cacheObject, ...)
            cacheObject <- getStructuredObject(keyObject, keys, value, permanent)
            createdOnce[[keys$cacheKey]] <<- TRUE
        }
    }

    # no result on any path, return NULL
    if(is.null(cacheObject)) return(NULL)

    # store cacheObject in our disk and/or ram cache
    if(from[1] == 'ram' && is.null(cache[[keys$cacheKey]])) cache[[keys$cacheKey]] <<- cacheObject
    if(permanent) { # refuse to overwrite new values with stale ones
        file <- getObjectFile(keys$cacheKey, create = TRUE)
        if(forceCreate || !file.exists(file)) saveRDS(cacheObject, file)
    }

    # the final return value
    cacheObject
}

#----------------------------------------------------------------------
# update the cached value in RAM and disk
#----------------------------------------------------------------------
set <- function(cacheObject, newValue=NULL){ # cacheKey as returned by the initial get call
    if(!is.null(newValue)) cacheObject$value <- newValue
    if(!is.null(cache[[cacheObject$cacheKey]])) cache[[cacheObject$cacheKey]] <<- cacheObject
    file <- getObjectFile(cacheObject$cacheKey, create = FALSE)
    if(file.exists(file)) { # i.e., is permanent
        cacheObject$timestamp <- Sys.time()
        saveRDS(cacheObject, file)
    }
    cacheObject # return the updated cacheObject, including its new timestamp
}

#----------------------------------------------------------------------
# clear the cache, and optionally the disk
#----------------------------------------------------------------------
clear <- function(cacheKeys = NULL, purgeFiles = FALSE) { # remove data/files only that are already in the RAM cache
    reportProgress(parentType, 'clearing cache')
    if(is.null(cacheKeys)) cacheKeys <- names(cache)
    if(purgeFiles) purgeFiles_(cacheKeys)
    for(cacheKey in cacheKeys) cache[[cacheKey]] <<- NULL   
}
purgeFiles_ <- function(cacheKeys){
    for(cacheKey in cacheKeys){
        file <- getObjectFile(cacheKey, create = FALSE)
        if(file.exists(file)) file.remove(file)
    }    
}
clearParentDir <- function(){ # remove all contents from parentDir from disk entirely
    reportProgress(parentType, 'clearing all cached files')
    if(dir.exists(parentDir)) unlink(parentDir, recursive = TRUE, force = TRUE)
    dir.create(parentDir)
}

#----------------------------------------------------------------------
# set return value
#----------------------------------------------------------------------
structure(
    list(
        get = get,
        set = set,
        clear = clear,
        clearParentDir = clearParentDir,
        getCacheKeys = getCacheKeys,
        cacheKeys = function() names(cache)
    ),
    class = class
) 

#----------------------------------------------------------------------
# END CLASS
#----------------------------------------------------------------------
}
#----------------------------------------------------------------------
