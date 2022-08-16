#----------------------------------------------------------------------
# utilities for populating an R Console dialog
#----------------------------------------------------------------------
rConsoleLink <- function(id, class = NULL){
    actionLink(id, NULL, icon = icon("r-project"), class = class)
}

#----------------------------------------------------------------------
# launch a stateful terminal emulator
#----------------------------------------------------------------------
showRConsole <- function(
    session,       # the session object of the calling module
    envir = NULL,  # the environment in which R code is evaluated; defaults to caller's environment
    label = NULL,  # text appended to "R Console" in the dialog header
    tall = FALSE,  # whether the dialog is currently extra-large (xl)
    wide = FALSE
){
    if(is.null(envir)) envir <- parent.frame()
    id <- "rConsoleDialog"
    nsId <- session$ns(id)
    cache <- rConsoleCache[[nsId]]
    onExit <- function(...){
        removeMatchingInputValues(session, id)
        rConsoleCache[[nsId]] <<- destroyModuleObservers(rConsoleCache[[nsId]])   
    }
    rConsoleCache[[nsId]] <<- rConsoleServer(
        id, 
        envir,
        code = cache$code,
        tall = if(!is.null(cache$tall)) cache$tall else tall,
        wide = if(!is.null(cache$wide)) cache$wide else wide,
        onExit = onExit
    )
    showUserDialog(
        HTML(paste(
            paste0("R Console", if(is.null(label)) "" else paste(" -", label)), 
            tags$i(
                id = "rConsoleSpinner",
                class = "fas fa-spinner fa-spin",
                style = "margin-left: 2em; color: #3c8dbc; display: none;"
            )
        )), 
        rConsoleUI(nsId),
        size = "l", 
        type = 'dismissOnly', 
        easyClose = FALSE,
        fade = FALSE,
        callback = onExit
    )
}
