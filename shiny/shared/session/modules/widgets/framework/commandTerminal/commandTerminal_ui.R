#----------------------------------------------------------------------
# static components for populating a command emulator dialog dialog
#----------------------------------------------------------------------

# module ui function
commandTerminalUI <- function(id, pipeline = NULL, action = NULL) {
    if(serverEnv$IS_SERVER) return(NULL)
    ns <- NS(id)
    isRuntime <- !is.null(pipeline) && !is.null(action)
    if(isRuntime) runtime <- paste(pipeline, action)
    tagList(
        uiOutput(ns("prompt")), # stylized command prompt
        fluidRow(
            column(
                width = 8, # text input where user types commands
                textInput(ns("command"), NULL, width = "100%")
            ),
            column(
                width = 2,
                style = "padding: 0;", # command timeout period
                textInput(ns("timeout"), NULL, placeholder = "Timeout (sec) [10]", width = "100%")
            ),
            column(
                width = 2, # enable command execution by button or Enter key
                bsButton(ns("execute"), "(Re)Execute", style = "primary")
            ),
            style = if(isRuntime) "" else "margin-bottom: 1em;"
        ),
        if(isRuntime) fluidRow(
            column(
                width = 12,
                checkboxInput(
                    ns('runtime'), # when requested, give user the option of running command in conda or container
                    paste0("execute command in '", runtime, "' runtime environment"), 
                    width = "100%"
                )
            )
         ) else "",
        tags$pre( # the pre-formatted command results, i.e., the command's output
            id = ns("results"), 
            "",
            class = "command-terminal command-terminal-lg"
        ),
        actionLink(ns("toggleWidth"),  "Toggle Width",  style = "margin-right: 15px;"),
        actionLink(ns("toggleHeight"), "Toggle Height", style = "margin-right: 15px;"),
        actionLink(ns("clear"), "Clear the results pane")
    )
}
