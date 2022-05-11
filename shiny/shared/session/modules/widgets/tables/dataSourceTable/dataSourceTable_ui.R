#----------------------------------------------------------------------
# static components for selecting one or more data sources in an analysis set
# most useful for apps that do not have samples
#----------------------------------------------------------------------

# module ui function
dataSourceTableUI <- function(id, title, width = 12, collapsible = FALSE) {
    
    # initialize namespace
    ns <- NS(id)
    
    # box with the table
    fluidRow(
        box(
            width = width,
            title = title,
            status = 'primary',
            solidHeader = TRUE,
            collapsible = collapsible,
            DTOutput(ns("table"))
        )
    )
}
