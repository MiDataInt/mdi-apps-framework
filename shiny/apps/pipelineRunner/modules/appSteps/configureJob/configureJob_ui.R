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

        # enable merging additional sample sources into this one
        tags$div(
            class = "text-block",
            sourceFileInputUI(ns('fileInput'), appName = 'pipelineRunner')
        ),

        # enable cold creation of a new job config file
        createJobFileUI(ns('create')),

        # tables of the sample sources and samples that are uploaded and ready
        conditionalPanel( condition = paste0("window['", ns('jobFiles-count'), "'] > 0"), 
            summaryTableUI(ns('jobFiles'), 'Job Configuration Files', width = 12, collapsible = TRUE),
        ),

        # second row inputs for selecting pipeline actions (if more than one)    
        span(
            class = "requiresJobFile",
            fluidRow(
                box(
                    width = 12,
                    title = "Working file actions", 
                    status = 'primary',
                    solidHeader = FALSE,
                    style = "padding: 0 0 10px 15px;",
                    actionLink(ns('discardChanges'), 'Discard Changes', style = "margin-right: 1rem;"),
                    uiOutput(ns('saveJobFileAsUI'), style = "display: inline-block; margin-right: 1rem; cursor: pointer;"), # nolint
                    uiOutput(ns('saveJobFileUI'), style = "display: inline-block;")
                ) 
            ),
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
            tags$p("Please Load or Create, and then click to select, a job configuration file to show its available options."), # nolint
            tags$p("You may load multiple configuration files into a job file group and save them together in a bookmark.") # nolint
        )
    ) 
}
