#----------------------------------------------------------------------
# configure the global environment
#----------------------------------------------------------------------
# global.R is sourced:
#     once when the web server starts, by run_server.R > Shiny::runApp()
#     once in every job execution child process (when running as a promise)
# thus, it is not re-sourced by page reload, but is by a call to stopApp()
# via the restart loop in run_server.R (it is not necessary to recall mdi::run())
#----------------------------------------------------------------------
# keep global.R minimal to avoid possibilities for session memory leaks
# and to allow maximum possibility for code updates without restarting servers
# script includes just what is needed to load the framework launch page
#----------------------------------------------------------------------
# the web server runs in .../mdi-apps-framework/shiny/shared
#----------------------------------------------------------------------
# message('--------- SOURCING shared/global.R ---------')

# steps only required in the parent process, not job execution children
if(isParentProcess){
    
    # check for required environment variables
    for (var in c('SHARED_DIR', 'MDI_DIR',
                  'SERVER_MODE', 'DEBUG',
                  'MAX_MB_RAM_BEFORE_START', 'MAX_MB_RAM_AFTER_END')){
        if(is.null(serverEnv[[var]])) stop(paste("missing variable:", var))
    }   
    
    # check for required directories
    for (dir in c('MDI_DIR', 'SHARED_DIR', 'LIBRARY_DIR')){
        if(!dir.exists(serverEnv[[dir]])) stop(paste('missing directory:', serverEnv[[dir]]))
    }
    
    # set counters for sessions over the lifetime of this app run
    # i.e. that were/are being handled by a call to Shiny runApp
    nShinySessions <- 0 # all sessions ever served since app startup
    nActiveShinySessions <- 0 # those sesssions that are currently running
    shinyId <- paste(Sys.time(), sample(1:1e8, 1)) # app identifier for the database tracking

    # load global constants
    source(file.path(serverEnv$SHARED_DIR, 'global', 'constants.R'), local = .GlobalEnv)
}

# load and attach initial Shiny load dependencies of the framework
# will fail if any package has not previously been installed into R
# NB: packages are loaded into an R process, i.e., at the server, not session, level
unloadRStudioPackages()
loadFrameworkPackages('yaml', isInit = TRUE) # since removed by unloadRStudioPackages()
frameworkPackages <- read_yaml( file.path('global', 'packages', 'packages.yml') )
loadMainPackages()
if(isParentProcess){
    loadAsyncPackages()
    loadDeveloperPackages()    
}
