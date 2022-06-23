#----------------------------------------------------------------------
# static components for link to load an MDI docs page in a new browser tab
#----------------------------------------------------------------------

# module ui function
documentationLinkUI <- function(id, isHeader = TRUE, isAppHeader = FALSE) {
    ns <- NS(id)
    if(isAppHeader) actionLink(
        ns('show'), 
        label = NULL, 
        icon = icon("book"),
        class = if(isHeader) "header-large-icon" else ""
    ) else span( 
        style = if(isHeader) "font-size: 0.7em; margin-left: 10px;" else "",
        actionLink(
            ns('show'), 
            NULL, 
            icon('book')
        )
    )
}
