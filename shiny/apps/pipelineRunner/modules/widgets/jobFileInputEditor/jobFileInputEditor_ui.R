#----------------------------------------------------------------------
# static components for editing a job configuration file with Shiny inputs
#----------------------------------------------------------------------

# module ui function
jobFileInputEditorUI <- function(id) {
    ns <- NS(id)
    tagList(
        # inputs for selecting pipeline actions (if more than one)               
        fluidRow(
            id = ns("actionSelectors"),
            box(
                width = 12,
                title = "Select one or more pipeline actions to execute", 
                status = 'primary',
                solidHeader = FALSE,
                style = "padding: 0 0 10px 15px;",
                checkboxGroupInput(ns('actions'), NULL, choices = NULL, inline = TRUE)
            ) 
        ),

        # input panels to enter/adjust job options by family
        fluidRow(
            box(
                width = 12,
                title = tags$span(
                    "Specify the job option values for each action", 
                    tags$span(
                        style = "margin-left: 0.5em; font-size: 0.9em;",
                        actionLink(ns("showRequiredOnly"), "Show required only"),
                        actionLink(ns("showAllOptions"), "Show all options", style = "display: none;")
                    )
                ),
                status = 'primary',
                solidHeader = FALSE,
                uiOutput(ns('optionFamilies'))
            )
        )         
    )
}
