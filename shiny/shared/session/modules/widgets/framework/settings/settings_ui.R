#----------------------------------------------------------------------
# static components for caching and only occasionally displaying a set 
# of input parameters for controlling how an application step or component behaves
#----------------------------------------------------------------------
# user click of a gear icon opens a dynamically populated popup
#----------------------------------------------------------------------
# usage in appStepUI:
#    standardSequentialTabItem(
#        HTML(paste( options$longLabel, settingsUI(ns('settings')) )),
#        options$leaderText,
#        ...
#----------------------------------------------------------------------

# module ui function
settingsUI <- function(id, isHeader=TRUE) {

    # initialize namespace
    ns <- NS(id)

    # return the UI contents
    # most typical usage places icon at the top of the page after the header
    span( 
        style = if(isHeader) "font-size: 0.8em; margin-left: 10px;" else NULL,
        actionLink(ns('gearIcon'), '', icon('cog', verify_fa = FALSE)) # apparently, "gear" is no longer the Font Awesome icon name
    )
}

# legacy name assignment
stepSettingsUI <- settingsUI
