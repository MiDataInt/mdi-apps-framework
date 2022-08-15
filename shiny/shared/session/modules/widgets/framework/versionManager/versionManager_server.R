#----------------------------------------------------------------------
# reactive components that support repo version examination and switching
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
versionManagerServer <- function(id, parentId, options) {
    moduleServer(id, function(input, output, session) {
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize the module
#----------------------------------------------------------------------
module <- "versionManager"
observers <- list() # for module self-destruction
spinnerSelector <- "#versionManagerSpinner"

#----------------------------------------------------------------------
# generate the summary display of all version information
#----------------------------------------------------------------------
output$pending <- renderUI({
    tags$pre(
        style = "max-height: 800px; overflow: auto;",
        paste(
            collapse = "\n", 
            capture.output(print(gitFrameworkStatus))
            # capture.output(print(reactiveValuesToList(gitStatusData)))
        )
    )
})


#----------------------------------------------------------------------
# set return value
#----------------------------------------------------------------------
list(
    observers = observers, # for use by destroyModuleObservers
    onDestroy = function() {
        list(  # return the module's cached state object
        )               
    }
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
