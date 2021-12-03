#----------------------------------------------------------------------
# static components to set pipeline job options
#----------------------------------------------------------------------

# module ui function
configureJobUI <- function(id, options) {

    # initialize namespace
    ns <- NS(id)
    
    # override missing options to defaults
    # options <- setDefaultOptions(options, stepModuleInfo$compositePlots)

    # incorporate options text into templates
    leaderText <- tagList(
        tags$p(HTML(options$leaderText))
    )
    
    # return the UI contents
    standardSequentialTabItem(
        # HTML(paste( options$longLabel, stepSettingsUI(ns('settings')) )),
        options$longLabel,
        leaderText,

        "Pending"
        
 
    )    
}
