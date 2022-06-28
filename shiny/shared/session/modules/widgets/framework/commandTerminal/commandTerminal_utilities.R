#----------------------------------------------------------------------
# utilities for populating a command terminal dialog
#----------------------------------------------------------------------
showCommandTerminal <- function(session, user = NULL, dir = NULL){
    dialogId <- "commandTerminalDialog"
    commandTerminalServer(
        dialogId, 
        user = user, 
        dir = dir
    )
    showUserDialog(
        HTML(paste(
            "Command Terminal Emulator", 
            tags$i(
                id = "commandTerminalSpinner",
                class = "fas fa-spinner fa-spin",
                style = "margin-left: 2em; color: #3c8dbc; display: none;"
            )
        )), 
        commandTerminalUI(session$ns(dialogId)),
        size = "l", 
        type = 'dismissOnly'
    ) 
}
