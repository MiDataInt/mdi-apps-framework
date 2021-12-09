#----------------------------------------------------------------------
# static components for a table that lists assembled job files
#----------------------------------------------------------------------

# module ui function
selectJobFilesUI <- function(id, width=12, collapsible=TRUE) {
    
    # initialize namespace
    ns <- NS(id)
    
    # the selection table
    summaryTableUI(ns('table'), 'Job Configuration Files', width = width, collapsible = collapsible)
}
