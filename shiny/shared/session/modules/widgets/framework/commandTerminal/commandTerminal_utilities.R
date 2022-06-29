#----------------------------------------------------------------------
# utilities for populating a command terminal emulator dialog
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# launch a stateful terminal emulator
#----------------------------------------------------------------------
commandTerminalCache <- list()
showCommandTerminal <- function(session, user = NULL, dir = NULL){
    id <- "commandTerminalDialog"
    nsId <- session$ns(id)
    cache <- commandTerminalCache[[nsId]]
    onExit <- function(...){
        removeMatchingInputValues(session, id)
        commandTerminalCache[[nsId]] <<- destroyModuleObservers(commandTerminalCache[[nsId]])   
    }    
    commandTerminalCache[[nsId]] <<- commandTerminalServer(
        id, 
        user = user, 
        dir = if(is.null(cache$dir)) dir else cache$dir,
        results = if(is.null(cache$results)) "" else cache$results,
        onExit = onExit
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
        commandTerminalUI(nsId),
        size = "l", 
        type = 'dismissOnly', 
        easyClose = FALSE,
        fade = FALSE,
        callback = onExit
    )
}

#----------------------------------------------------------------------
# manipulate the DOM inputs and working diretory
#---------------------------------------------------------------------
addCommandToHistory <- function(prefix, command = ""){
    runjs(paste0("addCommandToHistory('", prefix, "', '", command, "')"))
}
scrollCommandTerminalResults <- function(prefix){
    runjs(paste0("scrollCommandTerminalResults('", prefix, "')"))
}
changeTerminalDirectory <- function(dir, workingDir, prefix, ...){
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
    exit = function(parts, onExit = NULL, ...){ # close the modal on 'exit'
        if(!is.null(onExit)) onExit()
        removeModal()
    },

    # commands treated as aliases
    top = "top -bn 1 -u $USER",
    sq = "squeue -u $USER",
    ll = "ls -lh",
    mdi = file.path(serverEnv$MDI_DIR, "mdi")
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
