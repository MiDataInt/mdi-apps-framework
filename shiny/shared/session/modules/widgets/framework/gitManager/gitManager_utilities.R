#----------------------------------------------------------------------
# utilities for populating a dialog with git action buttons
#----------------------------------------------------------------------
gitManagerLink <- function(id){
    actionLink(
        id, 
        label = NULL, 
        icon = icon("github", verify_fa = FALSE),
        class = "header-large-icon",
        style = "font-size: 1.2em;"
    )
}

#----------------------------------------------------------------------
# launch a stateful code viewer/editor
#----------------------------------------------------------------------
showGitManager <- function(
    session
){
    id <- "gitManagerDialog" # gitManager is a single-instance link
    nsId <- session$ns(id)
    ns <- NS(nsId)
    gms <- gitManagerServer(id)    
    onExit <- function(...){
        removeMatchingInputValues(session, id)
        destroyModuleObservers(gms)   
    }
    showUserDialog(
        HTML(paste(
            "Git Repository Manager", 
            tags$i(
                id = "gitManagerSpinner",
                class = "fas fa-spinner fa-spin",
                style = "margin-left: 2em; color: #3c8dbc; display: none;"
            )
        )), 
        gitManagerUI(nsId),
        size = "l", 
        type = 'dismissOnly', 
        easyClose = FALSE,
        fade = FALSE,
        callback = onExit
    )
}
