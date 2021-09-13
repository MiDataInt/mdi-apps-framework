
#----------------------------------------------------------------------
# static components for a set of inputs that help users select
# a single data source associated with a SampleSet 
#----------------------------------------------------------------------

# module ui function
sampleSetSourceUI <- function(id) {

    # initialize namespace
    ns <- NS(id)

    # return the UI contents
    fluidRow(
        column(width=4, bookmarkInput('selectInput', ns('sampleSet'), 'Sample Set', "")),
        column(width=4, bookmarkInput('selectInput', ns('dataSource'), 'Data Source', ""))
    )
}

