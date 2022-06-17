#----------------------------------------------------------------------
# static components for link to load an MDI docs page in a new browser tab
#----------------------------------------------------------------------

# module ui function
documentationLinkUI <- function(id, isHeader = TRUE) {
    ns <- NS(id)
    span( 
        style = if(isHeader) "font-size: 0.7em; margin-left: 10px;" else NULL,
        actionLink(ns('show'), '', icon('book'))
    )
}
