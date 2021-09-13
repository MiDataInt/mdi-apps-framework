
#----------------------------------------------------------------------
# static components for a table that lists completed analysis jobs
#----------------------------------------------------------------------

# module ui function
selectAnalysesUI <- function(id, width=8, collapsible=TRUE) {
    
    # initialize namespace
    ns <- NS(id)
    
    # the selection table
    summaryTableUI(ns('table'), 'Completed Analyses', width=width, collapsible=collapsible)
 
}

