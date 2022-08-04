#----------------------------------------------------------------------
# reactive components for a link to open the developer addMdiTools dialog
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
addMdiToolsLinkServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        
#----------------------------------------------------------------------
# activate the config file editor in a modal popup
#----------------------------------------------------------------------
blur <- paste0("document.getElementById('", session$ns('open'),"').blur();")
observeEvent(input$open, {
    runjs(blur)
    dmsg("pending")
    # showAddMdiTools(session)
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
