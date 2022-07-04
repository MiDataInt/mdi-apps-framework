#----------------------------------------------------------------------
# static components to asynchronously fill an HTML div element
#----------------------------------------------------------------------

# module ui function
asyncDivUI <- function(id) {
    ns <- NS(id)
    tagList(
        uiOutput(ns("progress")),   
        uiOutput(ns("div"))
    )
}
