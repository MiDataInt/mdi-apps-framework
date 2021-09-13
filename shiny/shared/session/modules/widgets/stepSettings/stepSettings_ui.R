
#----------------------------------------------------------------------
# static components for caching and only occasionally displaying a set 
# of input parameters for controlling how an application step behaves
#----------------------------------------------------------------------
# user click of a gear icon opens a dynamically populated popup
#----------------------------------------------------------------------

# module ui function
stepSettingsUI <- function(id, isHeader=TRUE) {

    # initialize namespace
    ns <- NS(id)

    # return the UI contents
    # most typical usage places icon at the top of the page after the header
    span( 
        style = if(isHeader) "font-size: 0.8em; margin-left: 10px;" else NULL,
        actionLink(ns('gearIcon'), '', icon('gear'))
    )
}

