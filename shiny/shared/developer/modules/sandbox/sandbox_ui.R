
#----------------------------------------------------------------------
# static components that provide test code execution in an environment
# that has access to analyzed data sets but is entirely free-coded
#----------------------------------------------------------------------
# must never be enabled in server mode, as it allows arbitrary code execution 
#----------------------------------------------------------------------

# module ui function
sandboxUI <- function(id, options) {
    
    # initialize namespace
    ns <- NS(id)

    # incorporate options text into templates
    isAnalyzePragma <- !is.null(appStepNamesByType$analyze)
    leaderText <- tags$ul(
        tags$li("The Output, Plot and Table buttons execute your code for those output boxes."),
        tags$li("ls(sessionEnv) enumerates objects in the session environment."),
        if(isAnalyzePragma) tags$li("str(job()) shows the structure of the selected job's output.") else ""
    )

    # return the UI contents
    standardSequentialTabItem(
        "",
        leaderText,
        if(isAnalyzePragma) selectAnalysesUI(ns('schema')) else "",
        fluidRow(
            box(
                width=6,
                title = "Code Editor",
                status = 'primary',
                solidHeader = TRUE,
                bsButton(ns("codeButton"),   "Output",  style="success", class="margin-5"),
                bsButton(ns("plotButton"),   "Plot",    style="success", class="margin-5"),
                bsButton(ns("plotlyButton"), "Plotly",  style="success", class="margin-5"),
                bsButton(ns("tableButton"),  "Table",   style="success", class="margin-5"),
                actionLink(ns("ls_sessionEnv"), 'ls(sessionEnv)'),
                HTML("&nbsp;&nbsp;&nbsp;"),
                if(isAnalyzePragma) actionLink(ns("str_job"), 'str(job())') else "",
                tags$div(id=ns("code-editor"), style="height: 200px;")                  
            ),
            box(
                width=6,
                title = "Code Output",
                status = 'primary',
                solidHeader = TRUE,
                uiOutput(ns('codeOutput'))
            )
        ),
        fluidRow(
            box(
                width=6,
                title = "Plot Output",
                status = 'primary',
                solidHeader = TRUE,
                plotOutput(ns('plotOutput'))
            ),
            box(
                width=6,
                title = "Plotly Output",
                status = 'primary',
                solidHeader = TRUE,
                plotlyOutput(ns('plotlyOutput'))
            )
        ),
        fluidRow(
            box(
                width=12,
                title = "Table Output",
                status = 'primary',
                solidHeader = TRUE,
                DTOutput(ns('tableOutput'))        
            )
        )
    )    
}

