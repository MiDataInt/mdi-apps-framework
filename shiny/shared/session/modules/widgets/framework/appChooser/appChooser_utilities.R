#----------------------------------------------------------------------
# utilities for constructing a modal panel to cold start an app
#----------------------------------------------------------------------
showAppChooser <- function(
    session
){
    id <- "appChooserDialog"
    nsId <- session$ns(id)
    ns <- NS(nsId)
    appChooser <- appChooserServer(id)    
    onExit <- function(...){
        removeMatchingInputValues(session, id)
        destroyModuleObservers(appChooser)   
    }
    showUserDialog(
        "App Chooser", 
        appChooserUI(nsId),
        size = "l", 
        type = 'dismissOnly', 
        easyClose = FALSE,
        fade = FALSE,
        callback = onExit
    )
}
