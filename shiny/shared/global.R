#----------------------------------------------------------------------
# configure the global environment
#----------------------------------------------------------------------
# keep global.R minimal to avoid possibilities for session memory leaks
# and to allow maximum possibility for hotfixes and rolling updates without relaunching servers
# script includes just what is needed to load the framework launch page
#----------------------------------------------------------------------
# regardless of how Shiny::runApp was called, it runs in .../mdi-apps-framework/shiny/shared
#----------------------------------------------------------------------
# global.R is called:
#       once when the web server starts
#       once in every job execution child process (when running as a promise)
#----------------------------------------------------------------------

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
# NB: packages are loaded into an R process, i.e. at the server, not session, level
unloadRStudioPackages()
loadFrameworkPackages('yaml')
frameworkPackages <- read_yaml( file.path('global', 'packages', 'packages.yml') )
loadMainPackages()
if(isParentProcess){
    loadAsyncPackages()
    loadDeveloperPackages()    
}

# # collect the configuration of all apps
# appConfigs <- list()     # name = appName, value = config
# appFamilies <- list()    # name = appName, value = familyName
# appUploadTypes <- list() # name = uploadType, value = vector of appNames
# for(familyName in list.dirs(path = file.path(serverEnv$SHINY_DIR, 'apps'), full.names = FALSE, recursive = FALSE)){ 
#     for(appName in list.dirs(path = file.path(serverEnv$SHINY_DIR, 'apps', familyName), full.names = FALSE, recursive = FALSE)){ # nolint
#         configFile <- file.path(serverEnv$SHINY_DIR, 'apps', familyName, appName, 'config.yml')
#         if(!file.exists(configFile)) next
#         tryCatch(
#             { # tryCatch prevent framework failure on bad config
#                 appConfig <- read_yaml(configFile)
#                 appConfigs[[appName]] <- appConfig
#                 appConfigs[[appName]]$family <- familyName
#                 appFamilies[appName] <- familyName            
#                 if(!is.null(appConfig$uploadTypes)){ # again, catch and suppress bad app config
#                     for(uploadType in names(appConfig$uploadTypes)){ # associate apps with upload types
#                         if(is.null(appUploadTypes[[uploadType]])){
#                             appUploadTypes[[uploadType]] <- appName
#                         } else {
#                             appUploadTypes[[uploadType]] <- c(appUploadTypes[[uploadType]], appName)
#                         }
#                     }                
#                 }
#             }, 
#             error = function(e) print(e) 
#         )
#     }    
# }
