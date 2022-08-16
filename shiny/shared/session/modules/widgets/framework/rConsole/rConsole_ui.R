#----------------------------------------------------------------------
# static components for populating an R Console dialog
#----------------------------------------------------------------------

# module ui function
rConsoleUI <- function(id) {
    if(serverEnv$IS_SERVER) return("") # never allow command execution on public server
    ns <- NS(id)
    tagList(
        fluidRow(
            column(
                width = 5, # text input where user types commands
                tags$div(
                    id = ns("ace"), # the Ace editor for free code entry
                    tags$div(
                        bsButton(ns("codeButton"),   "Output",  style = "success", class = "margin-5"),
                        bsButton(ns("plotButton"),   "Plot",    style = "success", class = "margin-5"),
                        bsButton(ns("plotlyButton"), "Plotly",  style = "success", class = "margin-5"),
                        bsButton(ns("tableButton"),  "Table",   style = "success", class = "margin-5")
                    ),
                    tags$div(
                        class = "margin-5",
                        actionLink(ns("ls_sessionEnv"), 'ls(sessionEnv)')
                    ),
                    tags$div(
                        id = ns("codeEditor"), 
                        class = "r-console-editor-lg"
                    )   
                )  
            ),
            column(
                width = 7,
                fluidRow(
                    column(
                        width = 12,
                        tags$div(
                            id = ns("output"),
                            class = "r-console-pane r-console-lg",
                            style = "display: none;",
                            uiOutput(ns('codeOutput'))
                        )  
                    ),
                    column(
                        width = 12,
                        tags$div(
                            id = ns("plot"),
                            class = "r-console-pane r-console-lg",
                            style = "display: none;",
                            plotOutput(ns('plotOutput'))
                        )  
                    ),
                    column(
                        width = 12,
                        tags$div(
                            id = ns("plotly"),
                            class = "r-console-pane r-console-lg",
                            style = "display: none;",
                            plotlyOutput(ns('plotlyOutput'))
                        )  
                    ),
                    column(
                        width = 12,
                        tags$div(
                            id = ns("table"),
                            class = "r-console-pane r-console-lg",
                            style = "display: none;",
                            DTOutput(ns('tableOutput'))
                        )  
                    )
                )
            )
        ),
        actionLink(ns("toggleWidth"),  "Toggle Width",  style = "margin-right: 15px;"),
        actionLink(ns("toggleHeight"), "Toggle Height", style = "margin-right: 15px;")
    )
}
