#----------------------------------------------------------------------
# static components to asynchronously fill a bufferedTable
#----------------------------------------------------------------------

# module ui function
asyncTableUI <- function(id, title = NULL, downloadable = FALSE, ...) {
    ns <- NS(id)
    bufferedTableUI(
        ns("table"), 
        title = title,
        downloadable = downloadable,
        uiOutput(ns('progress')),   
        ...
    )
}
