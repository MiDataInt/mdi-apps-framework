#----------------------------------------------------------------------
# reactive components to list the working version and branch status of:
#   the mdi-apps-framework
#   the tools suite carring the running app
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
sibebarGitStatusServer <- function(id, suiteFn = function() NULL) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        module <- 'sibebarGitStatus' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize info boxes
#----------------------------------------------------------------------
#sibebarInfoBoxServer('version', serverEnv$PORTAL_VERSION)
# sibebarInfoBoxServer('repo', getCurrentGitRepo)
# sibebarInfoBoxServer('branch', function(){
#     sessionEnv$invalidateGitBranch() # trigger to update the UI
#     getCurrentGitHead()
# })


output$suite <- renderUI({
    suite <- suiteFn()
    if(is.null(suite)) return()
    if(is.null(suite$name)) return()
    sibebarInfoBoxUI(ns("suite"), suite$name)
})

repositoryStatus <- function(dir){
    if(is.null(dir)) return(NULL)
    tagList(
        if(serverEnv$IS_DEVELOPER) 
            tags$p(class = "sidebar-info-box-value-line", 
                   if(grepl('/developer-forks/', dir)) "developer-forks" else "definitive")
        else "",
        tags$p(class = "sidebar-info-box-value-line", 
               getGitHead(dir) )    
    )
}
sibebarInfoBoxServer('framework', function(...){
    # sessionEnv$invalidateGitBranch() # trigger to update the UI
    repositoryStatus(serverEnv$APPS_FRAMEWORK_DIR)
})
sibebarInfoBoxServer(ns("suite"), function(...){
    # sessionEnv$invalidateGitBranch() # trigger to update the UI
    suite <- suiteFn()
    if(is.null(suite)) return()
    if(is.null(suite$dir)) return()
    repositoryStatus(suite$dir)
})

# if isTag
# mdi-apps-framework
# v0.0.0 [v1.0.0]

# if isBranch
# mdi-apps-framework
# pre-release

# mdi-apps-framework
# myBranch

# wilsontelab-mdi-tools
# developer-fork
# workingBranch

# if isCommit (!isTag, !isBranch)
# mdi-apps-framework
# asf0s9af


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
