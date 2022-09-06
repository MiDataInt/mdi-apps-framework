#----------------------------------------------------------------------
# static components for a tabular view of data, with standardized special columns
# in particular, these are the tabular summaries at the bottom of appStep modules
#----------------------------------------------------------------------

# module ui function
summaryTableUI <- function(
    id, 
    title, 
    width, 
    collapsible = FALSE,
    skipFluidRow = FALSE
) {
    ns <- NS(id)
    box <- box(
        width = width,
        DTOutput(ns("table")),
        title = tags$span(
            tags$span(id = ns("title"), title),
            tags$span(id = ns("titleSuffix"), "")
        ),
        status = 'primary',
        solidHeader = TRUE,
        collapsible = collapsible
    )
    if(skipFluidRow) box else fluidRow(box)
}
