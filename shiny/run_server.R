#----------------------------------------------------------------------
# launch the MDI Stage 2 apps server
# sourced by mdi::run()
#----------------------------------------------------------------------
# thus, this script is re-sourced when MDI_FORCE_RESTART is set, followed
# by a call to stopApp() (stopApp() alone is not sufficient)
# if MDI_FORCE_REINSTALLATION is also set, git repos are pulled and R packages installed
#----------------------------------------------------------------------
# to summarize:
#   soft ui+server refresh  = triggered by page (re)load, (re)runs ui() and server() functions
#   hard ui+server refresh  = triggered by page (re)load, re-sources ui.r+server.R if changed, but not global.R
#   soft server restart     = triggered by stopApp(), re-sources everything except run_server.R <<<<< IMPACTS OTHER USERS <<<<<
#   hard server restart     = triggered by MDI_FORCE_RESTART+stopApp(), re-sources everything in apps framework
#   re-installation restart = triggered by MDI_FORCE_RESTART+MDI_FORCE_REINSTALLATION+stopApp(), also updates repos+packages
#----------------------------------------------------------------------
# the web server runs in .../mdi-apps-framework/shiny/shared
#----------------------------------------------------------------------
# message('--------- SOURCING run_server.R ---------')

# load environment variables
serverEnv <- as.list(Sys.getenv()) # thus, can access values as serverEnv$VARIABLE_NAME
setServerEnv <- function(name, default = NULL, type = as.character){
    serverEnv[[name]] <<- if(is.null(serverEnv[[name]])) {
        default
    } else {
        type(serverEnv[[name]])
    }
}

# adjust selected environment variables to logical
for(name in c('DEBUG', 'IS_DEVELOPER', 'IS_HOSTED', 'LAUNCH_BROWSER')) {
    serverEnv[[name]] <- as.logical(serverEnv[[name]])
}

# set structured environment variables based on mode
serverEnv$IS_WINDOWS  <- .Platform$OS.type != "unix"
serverEnv$IS_LOCAL    <- serverEnv$SERVER_MODE == 'local'
serverEnv$IS_REMOTE   <- serverEnv$SERVER_MODE == 'remote'
serverEnv$IS_NODE     <- serverEnv$SERVER_MODE == 'node'
serverEnv$IS_ONDEMAND <- serverEnv$SERVER_MODE == 'ondemand'
serverEnv$IS_SERVER   <- serverEnv$SERVER_MODE == 'server'
serverEnv$IS_LOCAL_BROWSER <- !serverEnv$IS_ONDEMAND # here, the browser runs on the server
serverEnv$REQUIRES_AUTHENTICATION <- serverEnv$IS_SERVER # other users already validated themselves via SSH, etc.

# set the interface the server listens to; only select cases listen beyond localhost
serverEnv$SERVER_PORT <- as.integer(serverEnv$SERVER_PORT)
serverEnv$HOST <- if(serverEnv$IS_LOCAL || serverEnv$IS_REMOTE || serverEnv$IS_ONDEMAND){
    "127.0.0.1"
} else if(serverEnv$IS_NODE || serverEnv$IS_SERVER) {
    "0.0.0.0"
} else {
    stop(paste('unknown server mode:', serverEnv$SERVER_MODE))    
}

# set properties based on whether server is publicly accessible or restricted access
if(serverEnv$IS_SERVER) { # public web server mode
    serverEnv$IS_DEVELOPER <- FALSE # !! never expose developer tools on a public server !!
    serverEnv$LAUNCH_BROWSER <- FALSE
    setServerEnv('MAX_MB_RAM_BEFORE_START', 1e3, as.integer)
    setServerEnv('MAX_MB_RAM_AFTER_END', 1e3, as.integer)
    serverEnv$CALLBACK_URL <- serverEnv$SERVER_URL
} else { # web server has highly restricted (often single-user) access
    setServerEnv('MAX_MB_RAM_BEFORE_START', 1e6, as.integer) # i.e. don't limit local start RAM
    setServerEnv('MAX_MB_RAM_AFTER_END', 1e3, as.integer)
    # serverEnv$SERVER_URL   <- paste0("http://localhost:", serverEnv$SERVER_PORT, "/") # cannot be 127.0.0.1 for Globus OAuth2 callback # nolint
    # serverEnv$CALLBACK_URL <- paste0("http://127.0.0.1:", serverEnv$SERVER_PORT, "/") # cannot be localhost for endpoint helper page action # nolint
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

#----------------------------------------------------------------------
# server auto-restarts when stopApp is called at session end, or upon config change
while(TRUE){
#----------------------------------------------------------------------

# load the Stage 2 apps config
source(file.path('global', 'packages', 'packages.R'))
loadFrameworkPackages(c('httr', 'yaml'))
serverConfig <- read_yaml(file.path(serverEnv$MDI_DIR, 'config', 'stage2-apps.yml'))
if(is.null(serverConfig$site_name)) serverConfig$site_name <- 'MDI'

# ensure that we have required server-level information for user authentication
serverEnv$IS_GLOBUS <- FALSE
serverEnv$IS_GOOGLE <- FALSE
serverEnv$IS_KEYED  <- FALSE
source(file.path('global', 'authentication', 'utilities.R')) 
source(file.path('global', 'authentication', 'sessionCache.R')) 
if(serverEnv$REQUIRES_AUTHENTICATION){
    if(is.null(serverConfig$access_control))
        stop("publicly addressable servers require an access_control declaration in config/stage2-apps.yml")
    else if(serverConfig$access_control == 'oauth2'){
        if(is.null(serverConfig$oauth2$host))
            stop("access_control mode 'oauth2' requires an oauth2$host declaration in config/stage2-apps.yml")
        if(!(serverConfig$oauth2$host %in% c('globus', 'google')))
            stop("unknown oauth2$host declaration in config/stage2-apps.yml; must be 'google' or 'globus'")
        if(is.null(serverConfig$oauth2$client$key) || 
           is.null(serverConfig$oauth2$client$secret))
            stop("invalid oauth2$client declaration in config/stage2-apps.yml; expect oauth2$client$key and oauth2$client$secret") # nolint
        serverEnv$IS_GLOBUS <- serverConfig$oauth2$host == 'globus'
        serverEnv$IS_GOOGLE <- serverConfig$oauth2$host == 'google'
    } else if(serverConfig$access_control == 'keys'){
        if(is.null(serverConfig$keys))
            stop("access_control mode 'keys' requires key declarations in config/stage2-apps.yml")
        serverEnv$IS_KEYED <- TRUE
    } else
        stop(paste("unknown access_control declaration:", serverConfig$access_control))
}
if(serverEnv$IS_GLOBUS || serverEnv$IS_GOOGLE) source(file.path('global', 'authentication', 'oauth2.R'))
if(serverEnv$IS_GLOBUS) source(file.path('global', 'authentication', 'globusAPI.R'))
if(serverEnv$IS_GOOGLE) source(file.path('global', 'authentication', 'googleAPI.R'))
if(serverEnv$IS_KEYED)  source(file.path('global', 'authentication', 'accessKey.R'))

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

# initialize git repository tracking
source(file.path('global', 'utilities', 'git.R'))
frameworkDir_ <- R.utils::getAbsolutePath(serverEnv$APPS_FRAMEWORK_DIR)
gitFrameworkStatus <- list(
    name = 'mdi-apps-framework',
    dir  = frameworkDir_,
    versions = getAllVersions(frameworkDir_)
)
gitFrameworkStatus$head <- getGitHead(gitFrameworkStatus)

# set the list of known apps (let Pipeline Runner load pipeline suites)
source(file.path('global', 'utilities', 'suites.R'))
appSuiteDirs <- getAppSuiteDirs()
appDirs <- getAppDirs(appSuiteDirs)
appUploadTypes <- getAppUploadTypes(appDirs) # uploadTypes recognized by installed apps; required prior to app load

# launch the Shiny app, a blocking action until/unless stopApp() is called
Sys.setenv(SHINY_SERVER_VERSION = '999.999.999') # suppress a baseless Shiny Server upgrade warning: https://rdrr.io/cran/shiny/src/R/runapp.R # nolint
# message('--------- CALLING run_server.R::runApp() ---------')
runApp(
    appDir = '.',
    host = serverEnv$HOST,   
    port = serverEnv$SERVER_PORT,
    launch.browser = serverEnv$LAUNCH_BROWSER
)

# check if framework has requested a hard server restart/reinstallation
# if not, loop will perform a soft restart by recalling runApp(), but not mdi::run()
if(Sys.getenv('MDI_FORCE_RESTART') != ""){
    install <- Sys.getenv('MDI_FORCE_REINSTALLATION') != ""
    Sys.setenv(MDI_FORCE_RESTART = "")
    Sys.setenv(MDI_FORCE_REINSTALLATION = "")
    mdi::run( # reinstalls and relaunches shiny server entirely anew (NB: can't use parallel::mcfork on Windows)
        mdiDir  = serverEnv$MDI_DIR,
        dataDir = serverEnv$DATA_DIR,
        hostDir = if(is.null(serverEnv$HOST_DIR) || 
                     serverEnv$HOST_DIR == "NULL" || 
                     serverEnv$HOST_DIR == "") NULL else serverEnv$HOST_DIR,  
        mode = serverEnv$SERVER_MODE,   
        install = install, 
        url = serverEnv$SERVER_URL,
        port = as.integer(serverEnv$SERVER_PORT),
        browser = as.logical(serverEnv$LAUNCH_BROWSER),
        debug = as.logical(serverEnv$DEBUG),
        developer = as.logical(serverEnv$IS_DEVELOPER)       
    )
    stop("no")
}

#----------------------------------------------------------------------
# end auto-restart loop
}
#----------------------------------------------------------------------
