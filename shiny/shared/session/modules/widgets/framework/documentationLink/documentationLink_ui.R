#----------------------------------------------------------------------
# static components for link to load an MDI docs page in a new browser tab
#----------------------------------------------------------------------

# module ui function
documentationLinkUI <- function(id, isHeader = TRUE, isAppHeader = FALSE, class = NULL) {
    ns <- NS(id)
    if(isAppHeader) actionLink(
        ns('show'), 
        label = NULL, 
        icon = icon("book"),
        class = if(!is.null(class)) class else if(isHeader) "header-large-icon" else ""
    ) else span( 
        class = if(!is.null(class)) class else if(isHeader) "header-link" else "",
        actionLink(
            ns('show'), 
            NULL, 
            icon('book')
        )
    )
}
