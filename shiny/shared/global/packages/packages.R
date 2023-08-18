#----------------------------------------------------------------------
# functions to check and load R package dependencies as listed in
#    packages.yml
# and installed into/read from .libPath(), which contains:
#    serverEnv$LIBRARY_DIR
#    plus any paths set in suite or base Singularity containers
#----------------------------------------------------------------------
runServerInitPackages <- c( # packages required prior to calling Shiny:runApp()
    "httr", 
    "yaml", 
    "storr", 
    "shiny"
)

#----------------------------------------------------------------------
# load and attach initial Shiny dependencies required to run the framework UI
#   must be loaded immediately on server launch
#----------------------------------------------------------------------
# because these packages are attached, code may use functions from them without
# namespace prefixes, e.g., 'observe' is fine, you do not need to use 'shiny::observe'
#----------------------------------------------------------------------
unloadMdiManagerPackages <- function(){
    # clear selected packages loaded by mdi-manager as they may be out of date
    tryCatch({
        sapply(c('miniCRAN'), unloadNamespace)
    }, error = function(e) NULL)
}
unloadRStudioPackages <- function(){
    # clear an RStudio session of these packages as they may be out of date
    # (these are loaded by RStudio, but not R)
    tryCatch({
        sapply(c('tinytex', 'tools', 'yaml', 'xfun'), unloadNamespace) # order is important
    }, error = function(e) NULL)
}
loadFrameworkPackages <- function(packages, isInit = FALSE){
    suppressWarnings({
        sapply(
            packages,
            function(package) {
                if(!isInit && package %in% runServerInitPackages) return(NULL)
                message(paste("loading package:", package))
                library(package, character.only = TRUE, warn.conflicts = FALSE)
            }
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
loadAsyncPackages <- function(session = NULL){
    if(!is.null(session)) startSpinner(session, 'loadAsyncPackages')
    loadFrameworkPackages(frameworkPackages$R$async)

    # switch to synchronous execution if only 1 core available
    if(availableCores() == 1) plan(sequential) # plan(cluster, workers = "localhost") 

    # otherwise use multicore on Linux, fall back to multisession on Windows
    else if(.Platform$OS.type == "unix") plan(multicore)
    else plan(multisession)
  
    if(!is.null(session)) stopSpinner(session)
}
