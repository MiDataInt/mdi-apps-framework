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
    for(key in names(persistentCache)){
        if(key %in% excludedKeys) next
        delta <- difftime(Sys.time(), persistentCache[[key]]$atime, units = "secs")
        if(delta > persistentCache[[key]]$ttl) persistentCache[[key]] <<- NULL
    }
}

# load content files and add resulting object to the persistent cache
loadPersistentFile <- function(
    file = NULL,     # specify the file to cache by path ...
    sourceId = NULL, # ... or source
    contentFileType = NULL, 
    #-----------------------
    force = FALSE, # force the object to be reloaded anew
    ttl = NULL,    # how long to cache the object after last access, in seconds
    #-----------------------
    sep = "\t", # parameters passed to fread
    header = TRUE,
    colClasses = NULL, # either a character vector or a function that returns one
    #-----------------------
    postProcess = NULL # a function applied to data after loading and before caching
){

    # adjust call parameters
    if(is.null(ttl)) ttl <- serverConfig$default_ttl 
    if(ttl > serverConfig$max_ttl) ttl <- serverConfig$max_ttl 
    if(is.null(file)) file <- getSourceFilePath(sourceId, contentFileType)  
    if(is.null(file)) stop("load cache error, missing file")      

    # check the cache for the requested file
    cleanPersistentCache(file)    
    if(!force && !is.null(persistentCache[[file]])) return(touchPersistentCache(file))
    reportProgress("loading persistent cache")
    reportProgress(file)   

    # load an RDS file into R
    isRdsFile <- endsWith(file, ".rds")
    rdsFile <- if(isRdsFile) file else paste(file, "rds", sep = ".")
    if(file.exists(rdsFile) && (isRdsFile || !force)){ 
        persistentCache[[file]] <<- readRDS(rdsFile)
        if(is.null(persistentCache[[file]]$ttl)) persistentCache[[file]] <<- list(
            data = persistentCache[[file]],
            ttl  = ttl
        )
        return(touchPersistentCache(file))
    }

    # load a non-RDS file into R
    if(!file.exists(file)) stop("load cache error, non-existent file")
    persistentCache[[file]] <<- list(
        data = if(endsWith(file, ".yml")){
            read_yaml(file)
        } else{
            if(is.function(colClasses)) colClasses <- colClasses()
            fread(
                file,
                sep = sep,        
                header = header,
                stringsAsFactors = FALSE,
                colClasses = colClasses
            )
        },
        ttl  = ttl
    )

    # allow the caller to perform post-processing on the loaded data
    if(!is.null(postProcess)) persistentCache[[file]]$data <<- postProcess(persistentCache[[file]]$data)

    # store the RDS version of the loaded file for faster future loads
    saveRDS(persistentCache[[file]], rdsFile)
    touchPersistentCache(file)
}