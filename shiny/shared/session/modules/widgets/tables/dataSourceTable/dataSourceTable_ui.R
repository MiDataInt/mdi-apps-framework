#----------------------------------------------------------------------
# static components for selecting one or more data sources in an analysis set
# most useful for apps that do not have samples
#----------------------------------------------------------------------

# module ui function
dataSourceTableUI <- function(id, title, width = 12, collapsible = FALSE, inFluidRow = TRUE, 
                              status = 'primary', solidHeader = TRUE) {
    
    # initialize namespace
    ns <- NS(id)
    
    # box with the table
    box_ <- box(
        width = width,
        title = title,
        status = status,
        solidHeader = solidHeader,
        collapsible = collapsible,
        DTOutput(ns("table"))
    )
    if(inFluidRow) fluidRow(box_) else box_
}
