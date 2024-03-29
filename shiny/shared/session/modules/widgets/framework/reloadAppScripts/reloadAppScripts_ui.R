#----------------------------------------------------------------------
# static components for developer link to reload certain app scripts without page reload
#----------------------------------------------------------------------

# module ui function
reloadAppScriptsUI <- function(id) {
    ns <- NS(id)
    actionLink(
        ns('reload'), 
        label = NULL, 
        icon = icon("sync", verify_fa = FALSE),
        class = "header-large-icon"
    )
}
