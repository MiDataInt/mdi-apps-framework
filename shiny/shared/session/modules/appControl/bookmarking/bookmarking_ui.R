#----------------------------------------------------------------------
# static components for app state save-and-recover tools
#----------------------------------------------------------------------

# module ui function
bookmarkingUI <- function(id, options) {
    
    # initialize namespace
    ns <- NS(id)

    # override missing options to defaults
    options <- setDefaultOptions(options, list(
        label = "Save Your Work",
        class = "sidebarBookmarking",
        shinyFiles = FALSE
    ))
    label <- paste0("-", options$label)
    
    # return a single button to initiate download...
    # ... to public server
    if(options$shinyFiles) serverBookmarkButtonUI(ns(id), label, class = options$class)
    # ...to local computer
    else downloadButton(ns(id), label, class = options$class)
}
