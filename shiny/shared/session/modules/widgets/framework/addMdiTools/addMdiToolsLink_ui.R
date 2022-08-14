#----------------------------------------------------------------------
# static components for a link to open the addMdiTools dialog
#----------------------------------------------------------------------

# module ui function
addMdiToolsLinkUI <- function(id) {
    ns <- NS(id)
    actionLink(
        ns('open'), 
        label = NULL, 
        icon = icon("plus", verify_fa = FALSE),
        class = "header-large-icon"
    )
}
