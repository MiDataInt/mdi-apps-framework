#----------------------------------------------------------------------
# static components to set pipeline job options
#----------------------------------------------------------------------

# module ui function
configureJobUI <- function(id, options) {
    if(serverEnv$SUPPRESS_PIPELINE_RUNNER) return(
        tags$h4("The Pipeline Runner app is disabled on this web server.")
    )

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
        HTML(paste( 
            options$longLabel, 
            documentationLinkUI(ns('docs')),
            settingsUI(ns('settings'))
        )), 
        leaderText,

        # enable merging additional sample sources into this one
        tags$div(
            class = "text-block",
            sourceFileInputUI(
                ns('fileInput'), 
                appName = 'pipelineRunner', 
                createButtonUI = createJobFileUI,
                priorPackages = FALSE
            )
        ),

        # name for bookmark files
        fluidRow(box(
            status = 'primary',
            solidHeader = TRUE,
            width = 4,
            title = tags$span(
                id = ns("setName"),
                'Configuration Set Name'
            ),
            textInput(ns('analysisSetName'), NULL, Sys.Date())
        )),

        # tables of job files that are loaded and ready
        conditionalPanel( condition = paste0("window['", ns('jobFiles-count'), "'] > 0"), 
            summaryTableUI(ns('jobFiles'), 'Job Configuration Files', width = 12, collapsible = TRUE),
        ),

        # UI sections that require a selected job file  
        span(
            class = "requiresJobFile",

            # inputs for saving and discarded job file edits
            fluidRow(
                box(
                    width = 12,
                    title = "Configuration file actions", 
                    status = 'primary',
                    solidHeader = FALSE,
                    style = "padding: 0 0 10px 15px;",
                    actionLink(ns('discardChanges'), 'Discard Changes', style = "margin-right: 2rem;"),
                    uiOutput(ns('saveJobFileAsUI'), style = "display: inline-block; margin-right: 2rem; cursor: pointer;"), # nolint
                    uiOutput(ns('saveJobFileUI'), style = "display: inline-block;")
                ) 
            ),

            # code sections for editing job files using Shiny inputs
            span(
                id = ns("uiBasedJobEditing"),
                jobFileInputEditorUI(ns("inputEditor"))               
            ),

            # code sections for editing job files using a script editor
            span(
                id = ns("textBasedJobEditing"),
                jobFileTextEditorUI(ns("textEditor"))
            )
        ),

        # message shown when no job file is selected
        div(
            class = "requiresJobFileMessage",
            style = "font-size: 1.1em; margin-left: 1em;",
            tags$p(HTML("Please <b>Create</b> or <b>Load</b>, and then <b>click to select</b>, a job configuration file to show its available options.")), # nolint
            tags$p("You may load multiple configuration files into a job file group and save them together in a bookmark.") # nolint
        )
    ) 
}
