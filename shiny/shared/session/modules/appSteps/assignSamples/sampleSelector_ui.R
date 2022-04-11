#----------------------------------------------------------------------
# static components to select one or more samples from a single sample set
#----------------------------------------------------------------------

# module ui function
sampleSelectorUI <- function(id) {

    # initialize namespace
    ns <- NS(id)
    
    # return the UI contents
    fluidRow( # fill whatever container we are in
        column(
            width = 6, 
            bookmarkInput('selectInput', ns('sampleSet'), 'Sample Set', "")
        ),
        column(
            width = 6, 
            tags$div(
                style = "margin-top: 24px;",
                bsButton(ns('selectSamples'), "Select Samples", style = "primary"),
                tags$span(
                    style = "margin-left: 10px;",
                    textOutput(ns('selectedSampleCount'), inline = TRUE)
                )
            )
        )
    )
}
