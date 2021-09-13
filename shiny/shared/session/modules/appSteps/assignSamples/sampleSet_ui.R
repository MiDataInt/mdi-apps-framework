
#----------------------------------------------------------------------
# static components for a single input to select a SampleSet
#----------------------------------------------------------------------

# module ui function
sampleSetUI <- function(id) {

    # initialize namespace
    ns <- NS(id)
    
    # return the UI contents
    fluidRow( # fill whatever container we are in
        column(width=12, bookmarkInput('selectInput', ns('sampleSet'), 'Sample Set', ""))
    )
}

