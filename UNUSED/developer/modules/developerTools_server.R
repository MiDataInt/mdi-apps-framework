
#----------------------------------------------------------------------
# reactive components that provide test code execution, file editing and
# git repository management to developers
#----------------------------------------------------------------------
# must never be enabled in server mode, as it allows arbitrary code 
# execution and alteration of files underlying the framework and apps
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
developerToolsServer <- function(id, options, bookmark, locks) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        module <- 'developerTools' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# activate sub-modules
#----------------------------------------------------------------------
#appConfigServer('appConfig', id, options)
sandboxServer('sandbox', id, options)
fileEditorServer('fileEditor', id, options)
# gitManagerServer('gitManager', id, options)

##----------------------------------------------------------------------
## define bookmarking actions
##----------------------------------------------------------------------
#observe({
#    bm <- getModuleBookmark(id, module, bookmark, locks)
#    req(bm)
#    data$list  <- bm$schema
#    data$selected <- bm$selected 
#})

#----------------------------------------------------------------------
# set return value
#----------------------------------------------------------------------
NULL

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------

