#----------------------------------------------------------------------
# reactive components to list the working version or branch status of:
#   the mdi-apps-framework
#   the tools suite carring the running app
#   the running app
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
sibebarGitStatusServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        module <- 'sibebarGitStatus' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# status utilities
#----------------------------------------------------------------------
repositoryStatus <- function(repo){
    if(is.null(repo)) return()
    if(is.null(repo$head)) return(NULL)
    x <- paste0(repo$name, ': ', repo$head[[repo$head$type]])
    if(isDeveloperFork(repo$dir)) x <- paste(x, '#')
    tags$p(x)
}

#----------------------------------------------------------------------
# fill the status information
# ------------------------------------------------------
sibebarInfoBoxServer("app", function(...){
    # sessionEnv$invalidateGitBranch() # trigger to update the UI    
    app <- gitStatusData$app
    if(is.null(app)) return()
    if(is.null(app$name)) return()
    tags$p( paste0(app$name, ': ', app$version) )
})
sibebarInfoBoxServer("suite", function(...){
    # sessionEnv$invalidateGitBranch() # trigger to update the UI
    repositoryStatus(gitStatusData$suite)
})
sibebarInfoBoxServer('framework', function(...){
    repositoryStatus(gitFrameworkStatus)
})

# #----------------------------------------------------------------------
# # update code elements used by framework
# # allows developers to see the results many of their changes
# # repeats the key steps of server.R in initializing a user session
# # if any confusion arises, alternative is simply to reload the web page
# #----------------------------------------------------------------------
# observeEvent(input$updateCode, {
#     startSpinner(session, 'sidebarStatus input$updateCode')
    
#     # resource all scripts (presumably including developer edits)
#     loadAllRScripts('global', recursive = TRUE)
#     loadAppScriptDirectory('session')
#     loadAppScriptDirectory(app$DIRECTORY)
    
#     # additional initialization steps
#     initializeDescendants()
#     initializeAppDataPaths()
#     locks <<- intializeStepLocks()
#     bookmark <<- bookmarkingServer('saveBookmarkFile', locks)
#     addRemoveModalObserver(input)
    
#     # re-activate module servers
#     sibebarStatusServer('frameworkStatus')
    
#     # NOT currently re-running step module servers, could/should it?
#     # at present any function inside module servers require a full page reload to take effect
#     # a reasons to be concerned is: would re-running servers add ADDITIONAL observers?

#     stopSpinner(session)
# })
    
#----------------------------------------------------------------------
# set return value
#----------------------------------------------------------------------
NULL

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
