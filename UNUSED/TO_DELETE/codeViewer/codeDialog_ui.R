#----------------------------------------------------------------------
# static components for an icon that loads a context-dependent code viewer/editor
#----------------------------------------------------------------------

# module ui function
codeDialogUI <- function(id, isHeader = TRUE) {
    ns <- NS(id)
    span( 
        style = if(isHeader) "font-size: 0.8em; margin-left: 10px;" else NULL,
        actionLink(ns('aceEditor'), label = NULL, icon = icon("code"), class = "")
    )
}
