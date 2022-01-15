#----------------------------------------------------------------------
# static components to list the working version and branch status of:
#   the mdi-apps-framework
#   the tools suite carring the running app
#----------------------------------------------------------------------

# module ui function
sibebarGitStatusUI <- function(id) {
    
    # initialize namespace
    ns <- NS(id)

    # return the UI contents
    fluidRow(
        class = "sidebar-status",  
        uiOutput(ns("suite")),
        sibebarInfoBoxUI(ns("framework"), 'mdi-apps-framework')
    )  
    # actionLink(ns('updateCode'), '-Update Code', icon = icon('sync-alt')),   
}
