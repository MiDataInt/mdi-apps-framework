
#----------------------------------------------------------------------
# functions to check and load R package dependencies as listed in
#    packages.yml
# and installed into/read from:
#       serverEnv$LIBRARY_DIR
#       i.e. portalDir/magc-portal-library/serverEnv$BioconductorRelease
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# load and attach initial Shiny dependencies required to run the framework UI
#   must be loaded immediately on server launch
#   will fail if any package has not previously been installed
#----------------------------------------------------------------------
# because these packages are attached, code may use functions from them without
# namespace prefixes, e.g. 'observe' is fine, you do not need to use 'shiny::observe'.
#----------------------------------------------------------------------
unloadRStudioPackages <- function(){
    # clear an RStudio session of these packages as they may be out of date
    # (these are loaded by RStudio, but not R)
    tryCatch({
        sapply(c('tinytex', 'tools', 'yaml', 'xfun'), unloadNamespace) # order is important
    }, error = function(e) NULL)
}
loadFrameworkPackages <- function(packages){
    suppressWarnings({
        sapply(
            packages,
            library,
            lib.loc = serverEnv$LIBRARY_DIR,
            character.only = TRUE
        )
    })
}
loadMainPackages <- function(){
    for(family in c('tools', 'data', 'graphics', 'framework')){
        loadFrameworkPackages(  frameworkPackages$R[[family]])
    }
}
loadDeveloperPackages <- function(){
    if(!serverEnv$IS_DEVELOPER) return(NULL)
    loadFrameworkPackages(frameworkPackages$R$developer$attached)
}

#----------------------------------------------------------------------
# load and attach packages required to launch asynchronous jobs
# use spinner since this can be slow on some local machines
#----------------------------------------------------------------------
loadAsyncPackages <- function(session=NULL){
    if(!is.null(session)) startSpinner(session, 'loadAsyncPackages')
    loadFrameworkPackages(frameworkPackages$R$async)

    # switch to effectively synchronous execution if only 1 core available (syntax per future package author)
    if(availableCores() == 1) plan(cluster, workers = "localhost") 

    # otherwise use multicore on Linux, fall back to multisession on Windows
    else if(.Platform$OS.type == "unix") plan(multicore)
    else plan(multisession)
  
    if(!is.null(session)) stopSpinner(session)
}

#----------------------------------------------------------------------
# load and attach packages for use during job execution (i.e. not for running the UI)
#   handled on demand by the server framework as loading is sometimes time/resource intensive
#----------------------------------------------------------------------

# install (if required) and load Bioconductor packages
# will fail if BiocManager has not previously been installed as part of the framework
installAndLoad_Bioconductor <- Vectorize(function(package){
    tryCatch({
        require(package, lib.loc=serverEnv$LIBRARY_DIR, character.only=TRUE) # if not installed, Bioconductor package is unknown to R # nolint
    }, warning = function(w){

        # sometimes there are other warnings about installed packages, e.g. regarding R version number
        # only install if the package is truly missing
        if(grepl('there is no package called', tolower(w$message))){
            if(serverEnv$IS_SERVER) stop(paste("missing required package:", package))
            BiocManager::install(
                package,
                lib     = serverEnv$LIBRARY_DIR,
                lib.loc = serverEnv$LIBRARY_DIR 
            )
            loadFrameworkPackages(package)   
        }
    })   
})

