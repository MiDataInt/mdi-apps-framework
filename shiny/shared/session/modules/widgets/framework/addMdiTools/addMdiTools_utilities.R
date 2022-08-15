#----------------------------------------------------------------------
# utilities for constructing a modal panel to add or create MDI tool suites and apps
#----------------------------------------------------------------------
showAddMdiTools <- function(
    session
){
    id <- "addMdiTools"
    nsId <- session$ns(id)
    ns <- NS(nsId)
    addMdiTools <- addMdiToolsServer(id)    
    onExit <- function(...){
        removeMatchingInputValues(session, id)
        destroyModuleObservers(addMdiTools)
        removeModal()
    }
    showUserDialog(
        paste("Add", if(serverEnv$IS_DEVELOPER) "and create" else "", "MDI tools"), 
        addMdiToolsUI(nsId),
        size = "m", 
        type = 'dismissOnly', 
        easyClose = FALSE,
        fade = FALSE,
        callback = onExit,
        removeModal = FALSE
    )
}