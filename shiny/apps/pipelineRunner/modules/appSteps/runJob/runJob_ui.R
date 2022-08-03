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
        style = "font-size: 0.8em; margin: auto 1rem;",
        actionLink(ns(id), NULL, icon = icon("sync", verify_fa = FALSE))
    )

    # return the UI contents
    standardSequentialTabItem(
        HTML(paste( options$longLabel, documentationLinkUI(ns('docs')) )),
        leaderText,
        selectJobFilesUI(ns('jobFiles')), 
        span(
            class = "requiresJobFile",

            # job file top-level action buttons
            fluidRow(
                lapply(list(
                    c('inspect',    'Inspect',  'primary', 'examine the parsed values of all job options'),
                    c('mkdir',      'Make Directory', 'primary', 'create all required output directory(s)'),
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

            # job status table for selected job configuration file
            fluidRow(
                style = "margin-top: 1.5em;",
                asyncTableUI(
                    ns('status'), 
                    width = 12,
                    title = tags$span(
                        refreshButton('refreshStatus'),                       
                        "Job Statuses"

                    ),
                    status = 'primary',
                    solidHeader = FALSE,
                    style = "padding: 0 0 10px 15px;",
                    collapsible = TRUE,
                    collapsed = FALSE
                )
            ),

            # task-level report/monitoring buttons for selected job
            uiOutput(ns("taskOptions")),

            # single-pane for viewing all command outputs
            fluidRow(
                style = "margin-top: 0.5em;",
                box(
                    width = 12,
                    title = tags$span(
                        refreshButton('refreshOutput'),                        
                        "Command Output"
                    ),
                    status = 'primary',
                    solidHeader = FALSE,
                    fluidRow(
                        id = ns("output-header"),
                        style = "margin: 0.5em 0;", 
                        column( # display of the command whose results are being viewed
                            width = 8,
                            style = "font-size: 1.1em;",
                            tags$strong(textOutput(ns('command'), inline = TRUE))
                        ),                     
                        column( # button to confirm execution of work after review of dry-run
                            width = 4,
                            uiOutput(ns('executeButton'))
                        )
                    ),
                    asyncDivUI(ns('output'))
                ) 
            )      
        ),

        # on-screen help
        div(
            class = "requiresJobFileMessage",
            style = "font-size: 1.1em; margin-left: 1em;",
            tags$p(HTML("Please <b>click to select</b> a job configuration file to launch and monitor its jobs."))
        )
    ) 
}
