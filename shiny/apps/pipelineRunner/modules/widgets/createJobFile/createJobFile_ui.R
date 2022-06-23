#----------------------------------------------------------------------
# static components for cold creation of a new Stage 1 job configuration file
#----------------------------------------------------------------------

# module ui function
createJobFileUI <- function(id, ...) {

    # initialize namespace
    ns <- NS(id)
    actionButton(ns("button"), "Create New", ...)
}
