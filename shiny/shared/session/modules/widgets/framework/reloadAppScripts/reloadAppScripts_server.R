#----------------------------------------------------------------------
# reactive components for developer link to reload  certain app scripts without page reload
#----------------------------------------------------------------------
# there are Shiny-based limitations to what code can be updated without a page reload
#   code external to modules (e.g., utilities) can be refreshed without a page reload
#   code changes within modules require a page reload to take effect
#----------------------------------------------------------------------
#     module <- function(){
#         message(1111) # if edited, will NOT change without page reload
#         sendMessage()
#     }
#     sendMessage <- function() message(2222) # if edited, WILL change if reloadAppScripts is clicked
#----------------------------------------------------------------------
# thus, efficient code editing/debugging will liberally use utility functions
# but changes to module components and structures always require page reloads
#----------------------------------------------------------------------
reloadAllAppScripts <- function(session, app){ # utility function also called by ace editor onDestroy
    startSpinner(session, "reloadAppScripts")
    #--------------------------------------------------------------
    loadAllRScripts('global', recursive = TRUE)
    loadAppScriptDirectory('session')
    #--------------------------------------------------------------
    if(!is.null(app$DIRECTORY)){
        loadAllRScripts(app$sources$suiteGlobalDir, recursive = TRUE)
        loadAppScriptDirectory(app$sources$suiteSessionDir)
        loadAppScriptDirectory(app$DIRECTORY)
    }
    #--------------------------------------------------------------
    stopSpinner(session, "reloadAppScripts")
}

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
reloadAppScriptsServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        
#----------------------------------------------------------------------
# activate the config file editor in a modal popup
#----------------------------------------------------------------------
blur <- paste0("document.getElementById('", session$ns('reload'),"').blur();")
observeEvent(input$reload, {
    runjs(blur)
    reloadAllAppScripts(session, app)
})

#----------------------------------------------------------------------
# return nothing
#----------------------------------------------------------------------
NULL

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
