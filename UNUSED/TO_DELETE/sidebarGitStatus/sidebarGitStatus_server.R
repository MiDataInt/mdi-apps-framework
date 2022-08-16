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
#----------------------------------------------------------------------
sibebarInfoBoxServer("app", function(...){   
    app <- gitStatusData$app
    if(is.null(app)) return()
    if(is.null(app$name)) return()
    tags$p( paste0(app$name, ': ', app$version) )
})
sibebarInfoBoxServer("suite", function(...){
    repositoryStatus(gitStatusData$suite)
})
sibebarInfoBoxServer('framework', function(...){
    repositoryStatus(gitFrameworkStatus)
})

#----------------------------------------------------------------------
# open the versionManager dialog
#----------------------------------------------------------------------
onclick("sidebarStatus", {
    showVersionManager(session)
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
