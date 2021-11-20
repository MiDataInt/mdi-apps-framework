#----------------------------------------------------------------------
# session initialization
#----------------------------------------------------------------------

# initialize a user session and record at framework and server levels
MbRAM_beforeStart <- sum(gc()[, 2]) # RAM dedicated to R before app started running
sessionEnv <- environment()
sessionEnv$invalidateGitBranch <- reactiveVal(0)
sessionStartTime <- Sys.time()
sessionNumber <- sample(1:1e8, 1)
sessionId <- paste(sessionStartTime, sessionNumber) # user session ID, like shinyId and serverId at higher scopes
sessionHash <- digest(sessionId) # NB: this is per _page_load_, unlike sessionKey which is per user encounter
sessionUrlBase <- paste('sessions', sessionHash, sep = "/")
sessionDirectory <- file.path(serverEnv$SESSIONS_DIR, sessionHash)
dir.create(sessionDirectory, showWarnings = FALSE)
nServerSessions       <<- nServerSessions + 1
nActiveServerSessions <<- nActiveServerSessions + 1    
nShinySessions        <<- nShinySessions + 1
nActiveShinySessions  <<- nActiveShinySessions + 1
userIP <- NULL
dataDirs <- list() # subdirectories in serverEnv$DATA_DIR

# instantiate session variables
app <- list(NAME = CONSTANTS$apps$launchPage) # parameters of the specific application
manifestTypes <- list()      # parameters of manifest files for different data classes
stepModuleInfo <- list()     # metadata about appStep modules, set by module.yml scripts
appStepNamesByType <- list() # lookup to find an app step name by the type(s) of its module
#analysisTypes <- list()     # parameters defining different types of data analyses
locks <- list()              # key relationships for data/UI integrity
bookmark <- NULL             # for saving and restoring app state
modalTmpFile <- NULL         # path to a file currently stored in www/tmp for loading into a modal
inlineScripts <- list()      # paths to scripts sourced by app step servers
oauth2UserData <- list()     # user info+tokens associated with external login (session-specific, not always required)
headerStatusData <- reactiveValues( # for UI display
    user = if(serverEnv$REQUIRES_AUTHENTICATION) "" else paste(Sys.getenv(c('USERNAME', 'USER')), collapse = ""),
    dataDir = serverEnv$DATA_DIR
)

# load support scripts required to run the framework
# note that scripts are loaded at the session, not the global, level
loadAllRScripts <- function(dir = ".", recursive = FALSE, local = NULL){
    if(!dir.exists(dir)) return(NULL)
    scripts <- list.files(dir, '\\.R$', full.names = TRUE, recursive = recursive)
    if(is.null(local)) local <- sessionEnv
    for(script in scripts) {
        if(!endsWith(script, '/global.R') && 
           !grepl('INLINE_ONLY', script, fixed = TRUE)) { # scripts intended to be sourced inline into other scripts
            # message(script)
            source(script, local = local)
        }
    }
}
loadAppScriptDirectory <- function(dir, local=NULL){
    loadAllRScripts(dir, recursive = FALSE, local = local)
    for(subDir in c('modules', 'types', 'ui', 'utilities')) {
        loadAllRScripts(paste(dir, subDir, sep = '/'), recursive = TRUE, local = local)
    }
}

loadAllRScripts('global', recursive = TRUE)
loadAppScriptDirectory('session')

# activate our custom page reset action; reloads the page as is, to update all code
observeEvent(input$resetPage, {
    updateQueryString("?resetPage=1", mode = "push") # clear the url
    refresh()
})
