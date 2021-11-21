#----------------------------------------------------------------------
# static components for a tabular view of analysis results, with download link
# typically in a viewResults module
#----------------------------------------------------------------------

# module ui function
resultsTableUI <- function(id, title, width=12) {
    
    # initialize namespace
    ns <- NS(id)
    
    # box with the table
    collapsibleBox(
        title = tags$p(
            class = "results-box-header-p",
            title,
            downloadButton(ns('download'), label = NULL, class = "results-box-header-button")
        ),
        width = width,
        DTOutput(ns('table'))
    )
}
