#----------------------------------------------------------------------
# static components to launch a pipeline job
#----------------------------------------------------------------------

# module ui function
runJobUI <- function(id, options) {

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
        options$longLabel,
        leaderText,
        selectJobFilesUI(ns('jobFiles')), 
        span(
            class = "requiresJobFile",
            fluidRow(
                box(
                    width = 12,
                    title = "Jobs status (mdi status ...)", 
                    status = 'primary',
                    solidHeader = FALSE,
                    style = "padding: 0 0 10px 15px;",
                    DTOutput(ns('statusTable'))
                ) 
            ),
            fluidRow(
                lapply(list(
                    c('refresh',    'Refresh',  'success'),
                    c('dryRun',     'Dry Run',  'info'),
                    c('submit',     'Submit',   'primary'),
                    c('rollback',   'Rollback', 'warning'),
                    c('purge',      'Purge',    'danger')
                ), function(x){
                    column(
                        width = 2,
                        bsButton(ns(x[1]), x[2], style = x[3], width = "100%")
                    )                
                })
            ),
            tags$div(
                style = "height: 400px; overflow: auto; margin-top: 1em;",
                verbatimTextOutput(ns('output'))   
            )        
        ),
        div(
            class = "requiresJobFileMessage",
            style = "font-size: 1.1em; margin-left: 1em;",
            tags$p("Please click to select a job configuration file to launch and monitor its jobs.")
        )
    ) 
}
