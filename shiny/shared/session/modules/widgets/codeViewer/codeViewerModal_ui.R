#----------------------------------------------------------------------
# static components that provide a modal code viewer for a specific file
#----------------------------------------------------------------------

# module ui function
codeViewerModalUI <- function(id) {
    ns <- NS(id)
    tags$span(
        style = "font-size: 14px; margin-left: 0.5em;",
        actionLink(ns('showCode'), "Show Code")
    )         
}
