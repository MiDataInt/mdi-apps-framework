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
    standardSequentialTabItem(
        # HTML(paste( options$longLabel, stepSettingsUI(ns('settings')) )),
        options$longLabel,
        leaderText,

        # top level inputs and outputs for suite, pipeline and job file
        box(
            width = 12,
            title = "Select the job's suite, pipeline and configuration file", 
            status = 'primary',
            solidHeader = FALSE,
            fluidRow(
                column( # top level selectors for suite and pipeline
                    width = 6, 
                    fluidRow(
                        column(width = 6, selectInput(ns('suite'),    'Suite',    choices = "")),
                        column(width = 6, selectInput(ns('pipeline'), 'Pipeline', choices = ""))                
                    ),
                    fluidRow( # show the working job file path on server
                        column(
                            width = 12, 
                            textOutput(ns("jobFilePath"))      
                        )
                    )                            
                ),
                column( # save action
                    width = 4,  
                    style = "padding-top: 10px; margin-top: 1em",      
                    # bsButton(ns("saveJobFile"), "Save Job Config", style = "success", class = "margin-5"),
                    serverServerFileButtonUI(ns("saveJobFile"), "Save Job Config", "myJob", ".yml"),
                    # actionLink(ns("resetEditPanel"), ' Reset Form'),
                    uiOutput(ns('saveJobFeedback'))
                )                   
            )
        ) %>% fluidRow(),

        # second row inputs for selecting pipeline actions (if more than one)    
        fluidRow(
            id = ns("actionSelectors"),
            box(
                width = 12,
                title = "Select one or more pipeline actions to execute", 
                status = 'primary',
                solidHeader = FALSE,
                style = "padding: 0 0 10px 10px;",
                checkboxGroupInput(ns('actions'), NULL, choices = NULL, inline = TRUE)
            ) 
        ),

        # panels to enter/adjust job options by family
        fluidRow(
            box(
                width = 12,
                title = "Specific job option values for each action", 
                status = 'primary',
                solidHeader = FALSE,
                uiOutput(ns('optionFamilies'))
            )
        )
    ) 
}
