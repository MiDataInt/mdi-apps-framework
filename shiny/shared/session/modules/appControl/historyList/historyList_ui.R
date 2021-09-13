
#----------------------------------------------------------------------
# static components for creating a cached, ordered list of historical items
#----------------------------------------------------------------------

# module ui function
historyListUI <- function(id) {
    
    # initialize namespace
    ns <- NS(id)

    # file upload input
    DTOutput(ns('table'))                   
}

