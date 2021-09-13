
#----------------------------------------------------------------------
# static components for loading a user's cached, recent bookmarks
#----------------------------------------------------------------------

# module ui function
bookmarkHistoryUI <- function(id) {
    
    # initialize namespace
    ns <- NS(id)

    # file upload input
    tags$div(
        fluidRow(
            class = "file-input-controls",
            column(width=12,
                historyListUI(ns('list'))
            )
        )
    )                       
}

