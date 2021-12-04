#----------------------------------------------------------------------
# reactive components to launch a pipeline job
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
runJobServer <- function(id, options, bookmark, locks) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        module <- 'runJob' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize installed pipelines
#----------------------------------------------------------------------



#----------------------------------------------------------------------
# define bookmarking actions
#----------------------------------------------------------------------
observe({
    bm <- getModuleBookmark(id, module, bookmark, locks)
    req(bm)
    # jobFile(bm$outcomes$jobFile)
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
        # jobFile = jobFile
    )
    # ,
    # isReady = reactive({ 
    #     getStepReadiness(fn = function() !is.null(jobFile()) ) 
    # })
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
