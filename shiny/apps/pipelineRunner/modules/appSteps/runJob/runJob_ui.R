#----------------------------------------------------------------------
# static components to launch and monitor pipeline jobs
#----------------------------------------------------------------------

# module ui function
runJobUI <- function(id, options) {
    if(serverEnv$SUPPRESS_PIPELINE_RUNNER) return("")

    # initialize namespace
    ns <- NS(id)
    
    # override missing options to defaults
    options <- setDefaultOptions(options, stepModuleInfo$runJob)

    # incorporate options text into templates
    leaderText <- tagList(
        tags$p(HTML(options$leaderText))
    )

    # reused elements
    refreshButton <- function(id) tags$span(
        style = "font-size: 0.8em; margin-left: 1rem;",
        actionLink(ns(id), NULL, icon = icon("sync"))
    )

    # return the UI contents
    standardSequentialTabItem(
        HTML(paste( options$longLabel, documentationLinkUI(ns('docs')) )),
        leaderText,
        selectJobFilesUI(ns('jobFiles')), 
        span(
            class = "requiresJobFile",
            fluidRow(
                lapply(list(
                    c('inspect',    'Inspect',  'primary', 'examine the complete set of job configuration options'),
                    c('submit',     'Submit',   'success', 'queue all required data analysis jobs'),
                    c('extend',     'Extend',   'success', 'queue only new or deleted/unsatisfied jobs'),
                    c('rollback',   'Rollback', 'danger', 'revert pipeline to the most recent prior log file'),
                    c('purge',      'Purge',    'danger',  'remove all log files associated with all jobs')
                ), function(x){
                    column(
                        width = 2,
                        bsButton(ns(x[1]), x[2], style = x[3], width = "100%")
                    )                
                })
            ),
            fluidRow(
                style = "margin-top: 1.5em;",
                box(
                    width = 12,
                    title = tags$span(
                        "Job Statuses", 
                        refreshButton('refreshStatus')
                    ),
                    status = 'primary',
                    solidHeader = FALSE,
                    style = "padding: 0 0 10px 15px;",
                    DTOutput(ns('statusTable'))
                ) 
            ),
            fluidRow(
                style = "margin-top: 1.5em;",
                box(
                    width = 12,
                    title = tags$span(
                        "Command Output", 
                        refreshButton('refreshOutput')
                    ),
                    status = 'primary',
                    solidHeader = FALSE,
                    fluidRow(
                        style = "margin: 0.5em 0;", 
                        column(
                            width = 8,
                            tags$strong(
                                style = "font-size: 1.1em;",                 
                                textOutput(ns('command'), inline = TRUE)
                            )
                        ),                     
                        column(
                            width = 4,
                            uiOutput(ns('executeButton'))
                        )
                    ),
                    tags$div(
                        class = "command-output-wrapper",
                        verbatimTextOutput(ns('output'))
                    )
                ) 
            )      
        ),
        div(
            class = "requiresJobFileMessage",
            style = "font-size: 1.1em; margin-left: 1em;",
            tags$p(HTML("Please <b>click to select</b> a job configuration file and launch and monitor its jobs."))
        )
    ) 
}
