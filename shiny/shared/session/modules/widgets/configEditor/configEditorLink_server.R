#----------------------------------------------------------------------
# reactive components for link to edit MDI installation config files
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
configEditorLinkServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        
#----------------------------------------------------------------------
# activate the config file editor in a modal popup
#----------------------------------------------------------------------
configFileContents <- reactiveVal()
observeEvent(input$open, {
    editorId <- paste0("configFileEditor", sample(1e8, 1))
    configFileContents( configFileEditorServer(editorId, id) )
    showUserDialog(
        "Configuration Editor", 
        configFileEditorUI(ns(editorId)), 
        callback = configFileContents()$save,
        size = "l", 
        type = 'saveCancel', 
        footer = NULL, 
        easyClose = FALSE
    )
})

#----------------------------------------------------------------------
# return nothing
#----------------------------------------------------------------------
NULL

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
