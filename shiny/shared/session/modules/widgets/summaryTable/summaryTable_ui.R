
#----------------------------------------------------------------------
# static components for a tabular view of data, with standardized special columns
# in particular, these are the tabular summaries at the bottom of appStep modules
#----------------------------------------------------------------------

# module ui function
summaryTableUI <- function(id, title, width, collapsible=FALSE) {
    
    # initialize namespace
    ns <- NS(id)
    
    # box with the table
    fluidRow(box(width=width,
        DTOutput(ns("table")),
        title = title,
        status = 'primary',
        solidHeader = TRUE,
        collapsible = collapsible
    ))
}

