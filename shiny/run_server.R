#----------------------------------------------------------------------
# launch the MDI Stage 2 apps server; sourced by mdi::run()
#----------------------------------------------------------------------

# load environment variables
serverEnv <- as.list(Sys.getenv()) # thus, can access values as serverEnv$VARIABLE_NAME
setServerEnv <- function(name, default = NULL, type = as.character){
    serverEnv[[name]] <<- if(is.null(serverEnv[[name]])) {
        default
    } else {
        type(serverEnv[[name]])
    }
}

# set structured environment variables based on mode
serverEnv$IS_SERVER   <- serverEnv$SERVER_MODE == 'server'
serverEnv$IS_LOCAL    <- serverEnv$SERVER_MODE == 'local'
serverEnv$IS_ONDEMAND <- serverEnv$SERVER_MODE == 'ondemand'
if(serverEnv$IS_LOCAL || serverEnv$IS_ONDEMAND){ # e.g., end user desktop or laptop
    serverEnv$HOST <- "127.0.0.1"
    setServerEnv('LAUNCH_BROWSER', TRUE, as.logical)
    setServerEnv('DEBUG', TRUE, as.logical)
    setServerEnv('IS_DEVELOPER', FALSE, as.logical)
    setServerEnv('MAX_MB_RAM_BEFORE_START', 1e6, as.integer) # i.e. don't limit local start RAM
    setServerEnv('MAX_MB_RAM_AFTER_END', 1e3, as.integer)
    serverEnv$SERVER_URL   <- paste0("http://localhost:", serverEnv$SERVER_PORT, "/") # cannot be 127.0.0.1 for Globus OAuth2 callback # nolint
    serverEnv$CALLBACK_URL <- paste0("http://127.0.0.1:", serverEnv$SERVER_PORT, "/") # cannot be localhost for endpoint helper page action # nolint
    FALSE
} else if(serverEnv$IS_SERVER) { # public web server mode
    serverEnv$HOST <- "0.0.0.0"
    serverEnv$LAUNCH_BROWSER <- FALSE
    setServerEnv('DEBUG', FALSE, as.logical)
    serverEnv$IS_DEVELOPER <- FALSE # !! never expose developer tools on a public server !!
    setServerEnv('MAX_MB_RAM_BEFORE_START', 1e3, as.integer)
    setServerEnv('MAX_MB_RAM_AFTER_END', 1e3, as.integer)
    serverEnv$CALLBACK_URL <- serverEnv$SERVER_URL
    TRUE
} else {
    stop(paste('unknown server mode:', serverEnv$SERVER_MODE))    
}

# set directories (framework runs from 'shared' directory that carries ui.R and server.R)
if(!dir.exists(serverEnv$MDI_DIR)) stop(paste('unknown directory:', serverEnv$MDI_DIR))
setServerDir <- function(name, parentDir, ..., check = TRUE, create = FALSE){
    serverEnv[[name]] <<- file.path(parentDir, ...)
    if(check  && !dir.exists(serverEnv[[name]])) stop(paste('missing directory:', serverEnv[[name]]))
    if(create && !dir.exists(serverEnv[[name]])) dir.create(serverEnv[[name]])
}
setServerDir('SHINY_DIR',  serverEnv$APPS_FRAMEWORK_DIR, 'shiny')
setServerDir('SHARED_DIR', serverEnv$SHINY_DIR, 'shared')
setServerDir('STORR_DIR',  serverEnv$DATA_DIR, 'storr', check = FALSE, create = TRUE)
setServerDir('CACHE_DIR',  serverEnv$DATA_DIR, 'cache', check = FALSE, create = TRUE)
setwd(serverEnv$SHARED_DIR)

# declare a version-specific R library from which all packages are loaded
.libPaths(serverEnv$LIBRARY_DIR)
if(serverEnv$DEBUG) message(paste("LIBRARY_DIR =", serverEnv$LIBRARY_DIR))

# set counters for sessions over the lifetime of this server
nServerSessions <- 0 # all sessions ever served since startup
nActiveServerSessions <- 0 # those sesssions that are currently running
serverId <- paste(Sys.time(), sample(1:1e8, 1)) # for keeping track of server instances in a database
if(serverEnv$DEBUG) message(paste('serverId', serverId))

# declare that we are the parent process ('future' child processes override this to FALSE)
isParentProcess <- TRUE

# establish Globus client credentials (order is important)
source(file.path('global', 'packages', 'packages.R'))
loadFrameworkPackages(c('httr', 'yaml'))
globusConfig <- tryCatch({
    read_yaml(file.path(serverEnv$MDI_DIR, 'config.yml'))
}, error = function(e) list(
    client     = list(key = NULL, secret = NULL),
    endpoint   = list(id = NULL),
    usersGroup = NULL
))
serverEnv$IS_GLOBUS <- !(is.null(globusConfig$client$key) ||
                         is.null(globusConfig$client$secret) ||
                         is.null(globusConfig$endpoint$id))
source(file.path('global', 'globus', 'globusAPI.R'))
source(file.path('global', 'globus', 'globusClient.R'))
source(file.path('global', 'globus', 'sessionCache.R'))
globusClientData <- if(serverEnv$IS_SERVER) {
    if(serverEnv$IS_DEVELOPER) message('getting client credentials')
    list( tokens = getGlobusClientCredentials() )
} else NULL

# initialize storr key-value on-disk storage
loadFrameworkPackages('storr')
serverEnv$STORR <- storr::storr_rds(serverEnv$STORR_DIR)

# declare sessions directory and clear any prior user sessions
loadFrameworkPackages('shiny')
addResourcePath('sessions', serverEnv$SESSIONS_DIR) # for temporary session files
invisible(unlink(
    list.files(serverEnv$SESSIONS_DIR, full.names = TRUE, include.dirs = TRUE),
    recursive = TRUE,
    force = TRUE
))

# web server debug assist (since harder to track log files on server)
PRINT_DEBUG_FILE <- 'PRINT_DEBUG.txt'
PRINT_DEBUG <- function(obj){
    x <- capture.output(print(obj))
    cat(x, file = PRINT_DEBUG_FILE, append = TRUE, sep = "\n")
}

# start the server
# with auto-restart when stopApp is called at session end
while(TRUE){
    runApp(
        appDir = '.',
        host = serverEnv$HOST,   
        port = serverEnv$SERVER_PORT,
        launch.browser = serverEnv$LAUNCH_BROWSER
    )
    dbDisconnect(sessionCacheDb)
}
