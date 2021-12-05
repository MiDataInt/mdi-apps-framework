#----------------------------------------------------------------------
# static components to set pipeline job options
#----------------------------------------------------------------------

# module ui function
configureJobUI <- function(id, options) {

    # initialize namespace
    ns <- NS(id)
    
    # override missing options to defaults
    options <- setDefaultOptions(options, stepModuleInfo$configureJob)

    # incorporate options text into templates
    leaderText <- tagList(
        tags$p(HTML(options$leaderText))
    )
    
    # return the UI contents
    padding <- "padding: 0 0 10px 10px;"
    standardSequentialTabItem(
        # HTML(paste( options$longLabel, stepSettingsUI(ns('settings')) )),
        options$longLabel,
        leaderText,

        # top level inputs and outputs for suite, pipeline and job file
        box(
            width = 12,
            title = "Set the job's suite, pipeline and configuration file", 
            status = 'primary',
            solidHeader = FALSE,
            fluidRow(
                column( # top level selectors for suite and pipeline
                    width = 6,
                    style = "margin-left: 10px;", 
                    fluidRow(
                        column(width = 6, selectInput(ns('suite'),    'Suite',    choices = "")),
                        column(width = 6, selectInput(ns('pipeline'), 'Pipeline', choices = ""))                
                    ),
                    fluidRow( # show the working job file path on server
                        style = "font-size: 1.1em;",
                        column(
                            width = 12, 
                            uiOutput(ns("jobFilePath"))      
                        )
                    )                            
                ),
                column( # save action
                    width = 4,  
                    style = "padding-top: 10px; margin-top: 1em",
                    uiOutput(ns("saveJobFileUI")),
                    uiOutput(ns('saveJobFeedback'))
                )                   
            )
        ) %>% fluidRow(),

        # second row inputs for selecting pipeline actions (if more than one)    
        span(
            class = "requiresJobFile",
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

            # panels to enter/adjust job options by family
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
        ),
        div(
            class = "requiresJobFileMessage",
            style = "font-size: 1.1em; margin-left: 1em;",
            "Please save a job configuration file to expose the available pipeline options."
        )
    ) 
}
