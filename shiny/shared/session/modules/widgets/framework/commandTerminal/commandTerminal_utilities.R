#----------------------------------------------------------------------
# utilities for populating a command terminal emulator dialog
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# launch a stateful terminal emulator
#----------------------------------------------------------------------
commandTerminalCache <- list()
showCommandTerminal <- function(
    session, 
    host = NULL,        # the host to ssh into when running terminal commands
    pipeline = NULL,    # as used in 'mdi <pipeline> shell --action <action> --runtime <runtime>''
    action = NULL,      #   to execute commands in a pipeline action's environment
    runtime = NULL,
    dir = NULL,         # the suggested directory in which to open the terminal
    forceDir = FALSE,    # always use `dir`, even if there is a cached value
    tall = FALSE,       # whether the dialog is currently extra-large (xl)
    wide = FALSE
){
    id <- "commandTerminalDialog"
    nsId <- session$ns(id)
    cache <- commandTerminalCache[[nsId]]
    onExit <- function(...){
        removeMatchingInputValues(session, id)
        commandTerminalCache[[nsId]] <<- destroyModuleObservers(commandTerminalCache[[nsId]])   
    }
    dir <- if(is.null(cache$dir) || forceDir) dir else cache$dir
    results <- if(is.null(cache$results) || is.null(cache$dir) || cache$dir != dir) "" else cache$results
    commandTerminalCache[[nsId]] <<- commandTerminalServer(
        id, 
        host = host,
        pipeline = pipeline,
        action = action,
        runtime = runtime,
        dir = dir,
        results = results,
        tall = if(!is.null(cache$tall)) cache$tall else tall,
        wide = if(!is.null(cache$wide)) cache$wide else wide,
        onExit = onExit
    )
    showUserDialog(
        HTML(paste(
            paste("Command Terminal Emulator", if(is.null(host)) "" else "(node)"), 
            tags$i(
                id = "commandTerminalSpinner",
                class = "fas fa-spinner fa-spin",
                style = "margin-left: 2em; color: #3c8dbc; display: none;"
            )
        )), 
        commandTerminalUI(
            nsId, 
            pipeline = pipeline, 
            action = action
        ),
        size = "l", 
        type = 'dismissOnly', 
        easyClose = FALSE,
        fade = FALSE,
        callback = onExit
    )
}

#----------------------------------------------------------------------
# manipulate the DOM inputs and working directory
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
