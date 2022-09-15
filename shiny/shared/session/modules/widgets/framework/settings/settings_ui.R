#----------------------------------------------------------------------
# static components for caching and only occasionally displaying a set 
# of input parameters for controlling how an application step or component behaves
#----------------------------------------------------------------------

# module ui function
settingsUI <- function(id, isHeader = TRUE, class = NULL) {

    # initialize namespace
    ns <- NS(id)

    # return the UI contents
    # most typical usage places icon at the top of the page after the header
    actionLink(
        ns('gearIcon'), 
        '', 
        icon('cog', verify_fa = FALSE),
        class = if(!is.null(class)) class else if(isHeader) "header-link" else NULL
    )
}

# legacy name assignment
stepSettingsUI <- settingsUI
