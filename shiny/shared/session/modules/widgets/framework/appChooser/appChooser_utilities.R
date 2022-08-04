#----------------------------------------------------------------------
# utilities for constructing a modal panel to select an app directly
#----------------------------------------------------------------------
showAppChooser <- function(
    session
){
    id <- "appChooserDialog"
    nsId <- session$ns(id)
    ns <- NS(nsId)
    appChooser <- appChooserServer(
        id
    )    
    onExit <- function(...){
        removeMatchingInputValues(session, id)
        destroyModuleObservers(appChooser)   
    }
    showUserDialog(
        "App Chooser", 
        appChooserUI(
            nsId
        ),
        size = "l", 
        type = 'okCancel', 
        easyClose = TRUE,
        fade = TRUE,
        callback = onExit # TODO: should execute callback on Cancel also
        # or more likely all links in the chooser close, only have a Dismiss button
    )
}

#----------------------------------------------------------------------
# app chooser support functions
#----------------------------------------------------------------------
