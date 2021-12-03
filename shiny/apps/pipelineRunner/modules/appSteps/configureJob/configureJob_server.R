#----------------------------------------------------------------------
# reactive components to set pipeline job options
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
configureJobServer <- function(id, options, bookmark, locks) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        module <- 'configureJob' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize module
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# handle the call from observeLoadRequest (note: this app doesn't take an input file)
#----------------------------------------------------------------------
loadSourceFile <- function(incomingFile, suppressUnlink = FALSE){
    reportProgress('launch', module)


    startSpinner(session, 'launch pipelineRunner')

    stopSpinner(session, 'launch pipelineRunner')

}

#----------------------------------------------------------------------
# define bookmarking actions
#----------------------------------------------------------------------
observe({
    bm <- getModuleBookmark(id, module, bookmark, locks)
    req(bm)
    # updateTextInput(session, 'analysisSetName', value = bm$outcomes$analysisSetName)
    # sources$list  <- bm$outcomes$sources
    # samples$list  <- bm$outcomes$samples
    # samples$names <- bm$outcomes$sampleNames
})

#----------------------------------------------------------------------
# set return values as reactives that will be assigned to app$data[[stepName]]
#----------------------------------------------------------------------
list(
    outcomes = list(
      
    ),
    loadSourceFile = loadSourceFile
    # ,
    # isReady = reactive({TRUE}) #reactive({ getStepReadiness(options$source, samples$list) })
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
