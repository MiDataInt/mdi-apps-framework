#----------------------------------------------------------------------
# static components for populating a command emulator dialog dialog
#----------------------------------------------------------------------

# module ui function
commandTerminalUI <- function(id) {
    if(serverEnv$IS_SERVER) return(NULL)
    ns <- NS(id)
    tagList(
        uiOutput(ns("prompt")),
        fluidRow(
            column(
                width = 8,
                textInput(ns("command"), NULL, width = "100%")
            ),
            column(
                width = 2,
                style = "padding: 0;",
                textInput(ns("timeout"), NULL, placeholder = "Timeout (sec) [10]", width = "100%")
            ),
            column(
                width = 2,
                bsButton(ns("execute"), "(Re)Execute", style = "primary")
            ),
            style = "margin-bottom: 1em;"
        ),
        tags$pre(
            id = ns("results"), 
            "",
            style = "height: 500px"
        ),
        actionLink(ns("clear"), "Clear the results pane")
    )
}
