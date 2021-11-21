#----------------------------------------------------------------------
# static components for app state save-and-recover tools
#----------------------------------------------------------------------

# module ui function
bookmarkingUI <- function(id, options) {
    
    # initialize namespace
    ns <- NS(id)

    # override missing options to defaults
    options <- setDefaultOptions(options, list(
        label = "-Save Your Work",
        class = ""
    ))
    
    # return a single button to initiate download
    downloadButton(ns(id), options$label, class = options$class)
}
