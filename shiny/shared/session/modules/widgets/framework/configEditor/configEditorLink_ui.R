#----------------------------------------------------------------------
# static components for link to edit MDI installation config files
#----------------------------------------------------------------------

# module ui function
configEditorLinkUI <- function(id) {
    ns <- NS(id)
    actionLink(
        ns('open'), 
        label = NULL, 
        icon = icon("cog", verify_fa = FALSE),
        class = "header-large-icon"
    )
}
