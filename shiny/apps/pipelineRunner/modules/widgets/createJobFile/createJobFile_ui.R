#----------------------------------------------------------------------
# static components for cold creation of a new Stage 1 job configuration file
#----------------------------------------------------------------------

# module ui function
createJobFileUI <- function(id) {

    # initialize namespace
    ns <- NS(id)

    tags$div(
        class = "text-block",
        fluidRow(
            class = "file-input-controls", 
            column(
                width = 5, 
                selectInput(ns('suite'),    'Suite',    choices = "")
            ),
            column(
                width = 4, 
                selectInput(ns('pipeline'), 'Pipeline', choices = "")
            ),
            column(
                width = 3, 
                style = "padding-top: 10px; margin-top: 1em",
                uiOutput(ns("createJobFileUI"))
            ) 
        )
    )
}
