#----------------------------------------------------------------------
# static components that support repo version examination and switching
#----------------------------------------------------------------------

# module ui function
versionManagerUI <- function(id) {
    ns <- NS(id)
    fluidRow(
        column(
            width = 12,
            uiOutput(ns("pending"))
        )
    )   
}
