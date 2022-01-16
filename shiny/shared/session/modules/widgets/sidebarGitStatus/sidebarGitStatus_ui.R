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
        tags$div(
            class = "sidebar-status-section",
            sibebarInfoBoxUI(ns("app"))
        ),
        tags$div(
            class = "sidebar-status-section",
            sibebarInfoBoxUI(ns("suite"))
        ), 
        tags$div(
            class = "sidebar-status-section",
            sibebarInfoBoxUI(ns("framework"))
        )  
    )   
}
