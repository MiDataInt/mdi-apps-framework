#----------------------------------------------------------------------
# static components to list the working version or branch status of:
#   the mdi-apps-framework
#   the tools suite carring the running app
#   the running app
#----------------------------------------------------------------------

# module ui function
sibebarGitStatusUI <- function(id) {
    
    # initialize namespace
    ns <- NS(id)

    # return the UI contents
    fluidRow(
        id = ns("sidebarStatus"),
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
