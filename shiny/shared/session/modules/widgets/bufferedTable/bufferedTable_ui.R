#----------------------------------------------------------------------
# static components to generate a DT/datatable that uses a buffer
# to minimize repeated redraws of the table
#----------------------------------------------------------------------

# module ui function
bufferedTableUI <- function(id) {
    ns <- NS(id)
    DTOutput(ns('table'))    
}
