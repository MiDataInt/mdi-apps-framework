#----------------------------------------------------------------------
# utilities for populating a command terminal dialog
#----------------------------------------------------------------------
commandTerminalCache <- list()
commandTerminalCounter <- 0
showCommandTerminal <- function(session, user = NULL, dir = NULL){
    id <- "commandTerminalDialog"
    instanceId <- paste(id, commandTerminalCounter, sep = "_")
    id <- session$ns(id)
    cachedDir <- commandTerminalCache[[id]]
    dir <- if(is.null(cachedDir)) dir else cachedDir
    commandTerminalCache[[id]] <<- commandTerminalServer(
        instanceId, 
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
        commandTerminalUI(session$ns(instanceId)),
        size = "l", 
        type = 'dismissOnly', 
        easyClose = FALSE,
        callback = function(...) {
            commandTerminalCache[[id]] <<- commandTerminalCache[[id]]$destroy()
        }
    )
    commandTerminalCounter <<- commandTerminalCounter + 1
}
addCommandToHistory <- function(prefix, command = ""){
    runjs(paste0("addCommandToHistory('", prefix, "', '", command, "')"))
}
scrollCommandTerminalResults <- function(prefix){
    runjs(paste0("scrollCommandTerminalResults('", prefix, "')"))
}
changeTerminalDirectory <- function(dir, workingDir, prefix){
    wd <- isolate({ workingDir() })    
    root <- if(serverEnv$IS_WINDOWS) {
        drive <- toupper(strsplit(wd, "")[[1]][1])
        paste0(drive, ":/")
    } else "/"
    if(!startsWith(toupper(dir), root)){
        dir <- switch(
            dir,
            "."  = wd,
            ".." = dirname(wd),
            "~"  = serverEnv$HOME,
            file.path(wd, dir)
        )
    } 
    req(dir.exists(dir))
    workingDir(dir)
    addCommandToHistory(prefix, paste("cd", dir))
    scrollCommandTerminalResults(prefix)
    NULL
}

#----------------------------------------------------------------------
# handle command intercepts, i.e. override certain commands for use in the emulator
#----------------------------------------------------------------------
terminalCommandIntercepts <- list(

    # commands not executed via system() function
    cd = function(parts, workingDir, ...) {
        req(parts[2])
        parts[1] <- NA
        dir <- paste(na.omit(parts), collapse = " ")
        changeTerminalDirectory(dir, workingDir, ...)
    },

    # commands treated as aliases
    top = "top -bn 1 -u $USER",
    sq = "squeue -u $USER",
    ll = "ls -lh"
)
interceptTerminalCommands <- function(command, ...){
    command <- trimws(command)
    req(command)
    parts <- strsplit(command, " ")[[1]]
    intercept <- terminalCommandIntercepts[[parts[1]]]
    if(is.null(intercept)) return(command)
    if(is.character(intercept)) {
        parts[1] <- intercept
        return(paste(parts, collapse = " "))
    }
    intercept(parts, ...)
}
