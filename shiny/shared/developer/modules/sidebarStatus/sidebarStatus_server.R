#----------------------------------------------------------------------
# reactive components list version and git branch status in the sidebar
# when working as a developer
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
sibebarStatusServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        module <- 'sibebarStatus' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize info boxes
#----------------------------------------------------------------------
#sibebarInfoBoxServer('version', serverEnv$PORTAL_VERSION)
sibebarInfoBoxServer('repo', getCurrentGitRepo)
sibebarInfoBoxServer('branch', function(){
    sessionEnv$invalidateGitBranch() # trigger to update the UI
    getCurrentGitBranch()
})

#----------------------------------------------------------------------
# update code elements used by framework
# allows developers to see the results many of their changes
# repeats the key steps of server.R in initializing a user session
# if any confusion arises, alternative is simply to reload the web page
#----------------------------------------------------------------------
observeEvent(input$updateCode, {
    startSpinner(session, 'sidebarStatus input$updateCode')
    
    # resource all scripts (presumably including developer edits)
    loadAllRScripts('global', recursive=TRUE)
    loadAppScriptDirectory('session')
    loadAppScriptDirectory(app$DIRECTORY)
    
    # additional initialization steps
    initializeDescendants()
    initializeAppDataPaths()
    locks <<- intializeStepLocks()
    bookmark <<- bookmarkingServer('saveBookmarkFile', locks)
    addRemoveModalObserver(input)
    
    # re-activate module servers
    sibebarStatusServer('frameworkStatus')
    
    # NOT currently re-running step module servers, could/should it?
    # at present any function inside module servers require a full page reload to take effect
    # a reasons to be concerned is: would re-running servers add ADDITIONAL observers?

    stopSpinner(session)
})
    
#----------------------------------------------------------------------
# set return value
#----------------------------------------------------------------------
NULL

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
