#----------------------------------------------------------------------
# static components for a shell module for viewing analyis results
# the module is generic and can support any type of completed analysis
#----------------------------------------------------------------------
# module follows the archetype of upper form + lower contents set
#   here, the upper form is simply an analysis schema selection table
#   output is more complex, variable and specified by analysis types
#----------------------------------------------------------------------

# module ui function
viewResultsUI <- function(id, options) {
    
    # initialize namespace
    ns <- NS(id)
    
    # override missing options to defaults
    options <- setDefaultOptions(options, stepModuleInfo$view)

    # incorporate options text into templates
    title <- HTML( paste( options$longLabel, settingsUI(ns('settings')) ) )
    leaderText <- tags$p(HTML("<strong>Select an analysis</strong> whose results you would like to view."))

    # return the UI contents
    standardSequentialTabItem(
        title,
        leaderText,
        selectAnalysesUI(ns('schema')), # table of all available analyses
        uiOutput(ns('viewerPanels'))    # one UI set per analysis type (hidden until needed)
    )    
}
