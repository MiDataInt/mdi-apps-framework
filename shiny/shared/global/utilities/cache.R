#----------------------------------------------------------------------
# a persistent RAM cache for R data objects that operates at the server level
# and is therefore available to all sessions, subject to lifecycle policies
#----------------------------------------------------------------------
# uses the persistentCache list defined in run_server.R
#----------------------------------------------------------------------

# update the cache access time and return the cache key
# all functions that load and/or access the cache should return touchPersistentCache()
touchPersistentCache <- function(key){
    persistentCache[[key]]$atime <<- Sys.time() # last access time
    key # return the key into persistentCache for use by caller
}

# clear all cached items that have exceed their time-to-live (TTL)
cleanPersistentCache <- function(excludedKeys = "__NO_EXCLUSIONS__"){  
    keys <- names(persistentCache)
    keys <- keys[!(keys %in% excludedKeys)]
    if(length(keys) == 0) return(NULL)
    now <- Sys.time()
    deltas <- sapply(keys, function(key){
        delta <- difftime(now, persistentCache[[key]]$atime, units = "secs")
        if(delta > persistentCache[[key]]$ttl) persistentCache[[key]] <<- NULL
        delta
    })

    # even if TTL not exceeded delete the oldest items until max cache size is honored
    if(serverConfig$max_cache_bytes == 0) return(NULL)
    cacheSize <- object.size(persistentCache)
    if(cacheSize < serverConfig$max_cache_bytes) return(NULL)
    mostOutOfDate <- order(deltas)
    i <- length(mostOutOfDate)
    while(cacheSize > serverConfig$max_cache_bytes){
        j <- mostOutOfDate[i]
        key <- keys[j]
        persistentCache[[key]] <<- NULL
        if(i == 1) return(NULL)
        i <- i - 1
        cacheSize <- object.size(persistentCache)
    }
}

# load content files and add resulting object to the persistent cache
loadPersistentFile <- function(
    file = NULL,     # specify the file to cache by path ...
    sourceId = NULL, # ... or source
    contentFileType = NULL, 
    #-----------------------
    force = FALSE,   # force the object to be reloaded anew from the disk file
    unlink = FALSE,  # delete the file prior to loading the cache; requires options `force` and `create`
    ttl = NULL,      # how long to cache the object after last access, in seconds
    silent = NULL,   # fail silently and return NULL if file not found
    #-----------------------
    sep = "\t",      # parameters passed to fread
    header = TRUE,
    colClasses = NULL, # either a character vector or a function that returns one
    #-----------------------
    create = NULL, # a function(file) called to create a non-existent file prior to RAM caching
    postProcess = NULL, # a function applied to data after loading and before caching
    cacheAsRds = TRUE, # set to FALSE to skip saving the data as an RDS file, which can be slow to write (but faster to reload)
    #-----------------------
    session = NULL, # optional control over the spinner shown only for files not already in cache
    spinnerMessage = NULL,
    log = FALSE,
    #-----------------------
    ... # additional argument passed to fread
){
    # adjust call parameters
    if(is.null(ttl)) ttl <- serverConfig$default_ttl 
    if(ttl > serverConfig$max_ttl) ttl <- serverConfig$max_ttl 
    if(is.null(file)) file <- getSourceFilePath(sourceId, contentFileType)  
    if(is.null(file) || length(file) == 0) {
        if(!silent) stop("load cache error, missing file")   
        return(NULL) 
    }
 
    # check the cache for the requested file
    cleanPersistentCache(file)  
    if(!force && !is.null(persistentCache[[file]])) return(touchPersistentCache(file))
    if(log){
        reportProgress("loading persistent cache")
        reportProgress(file) 
    } 
    # if(!is.null(spinnerMessage)) startSpinner(session, message = spinnerMessage)

    # force a full reload of the file too, not just the RAM cache
    if(unlink && file.exists(file)) unlink(file)

    # load an RDS file into R
    isRdsFile <- endsWith(file, ".rds")
    rdsFile <- if(isRdsFile) file else paste(file, "rds", sep = ".")
    loadRdsFile <- function(){
        persistentCache[[file]] <<- readRDS(rdsFile)
        isList <- is.list(persistentCache[[file]]) && !is.data.frame(persistentCache[[file]])
        if(!isList || is.null(persistentCache[[file]]$ttl)) persistentCache[[file]] <<- list(
            data = if(!is.null(postProcess)) postProcess(persistentCache[[file]]) else persistentCache[[file]],
            ttl  = ttl
        )
        # if(!is.null(spinnerMessage)) stopSpinner(session)
        touchPersistentCache(file)
    }
    if(file.exists(rdsFile) && (isRdsFile || !force)) return( loadRdsFile() )

    # load a non-RDS file into R
    if(!file.exists(file)) {
        if(is.null(create)){
            if(!silent) stop("load cache error, non-existent file") 
            # if(!is.null(spinnerMessage)) stopSpinner(session)  
            return(NULL) 
        } else {
            create(file)
            if(isRdsFile) return( loadRdsFile() )
        }
    }
    persistentCache[[file]] <<- list(
        data = if(endsWith(file, ".yml")){
            read_yaml(file)
        } else{
            if(is.function(colClasses)) colClasses <- colClasses()
            fread(
                file,
                sep = sep,
                header = header,
                colClasses = colClasses,
                ...
            )
        },
        ttl  = ttl
    )

    # allow the caller to perform post-processing on the loaded data
    if(!is.null(postProcess)) persistentCache[[file]]$data <<- postProcess(persistentCache[[file]]$data)

    # store the RDS version of the loaded file for faster future loads
    if(cacheAsRds) saveRDS(persistentCache[[file]], rdsFile)
    # if(!is.null(spinnerMessage)) stopSpinner(session)
    touchPersistentCache(file)
}
