#----------------------------------------------------------------------
# static components list version and git branch status in the sidebar
# when working as a developer
#----------------------------------------------------------------------

# module ui function
sibebarStatusUI <- function(id) {
    
    # initialize namespace
    ns <- NS(id)

    # return the UI contents
    fluidRow(
        class = "sidebar-status",
        actionLink(ns('updateCode'), '-Update Code', icon = icon('sync-alt')),          
        sibebarInfoBoxUI(ns("version"), 'portal version'),
        sibebarInfoBoxUI(ns("repo"), 'git repo'),
        sibebarInfoBoxUI(ns("branch"), 'git branch')
    )  
}
