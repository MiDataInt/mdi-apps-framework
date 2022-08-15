#----------------------------------------------------------------------
# utilities for populating a dialog with the version manager
#----------------------------------------------------------------------
showVersionManager <- function(
    session
){
    id <- "versionManagerDialog"
    nsId <- session$ns(id)
    ns <- NS(nsId)
    vms <- versionManagerServer(id)    
    onExit <- function(...){
        removeMatchingInputValues(session, id)
        destroyModuleObservers(vms)   
    }
    showUserDialog(
        HTML(paste(
            "Version Manager", 
            tags$i(
                id = "versionManagerSpinner",
                class = "fas fa-spinner fa-spin",
                style = "margin-left: 2em; color: #3c8dbc; display: none;"
            )
        )), 
        versionManagerUI(nsId),
        size = "l", 
        type = 'dismissOnly', 
        easyClose = FALSE,
        fade = FALSE,
        callback = onExit
    )
}
