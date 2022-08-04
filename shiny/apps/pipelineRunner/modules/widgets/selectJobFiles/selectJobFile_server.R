#----------------------------------------------------------------------
# reactive components for a table that lists assembled job files
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
selectJobFilesServer <- function(id, parentId, parentOptions) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        parentNS <- NS(parentId)
        module <- 'selectJobFiles' # for reportProgress tracing
#----------------------------------------------------------------------
    
#----------------------------------------------------------------------
# define session-level and module-level variables
#----------------------------------------------------------------------

# initialize analysis schema
summaryTemplate <- data.frame(
    Suite       = character(),
    Pipeline    = character(),
    FileName    = character(),
    Directory   = character(),
        stringsAsFactors = FALSE
)
jobFilesTable <- summaryTableServer(
    id = 'table', # NOT ns(id) when nesting modules!
    parentId = id,
    stepNumber = parentOptions$stepNumber,
    stepLocks = locks[[parentId]],
    sendFeedback = NULL,
    template = summaryTemplate,
    type = 'shortList'
)

#----------------------------------------------------------------------
# reactively update the job files summary table
#----------------------------------------------------------------------
jobFilesSource <- getAppStepByType('configureJob')$outcomes$jobFiles
observe({
    jobFiles <- jobFilesSource()
    req(jobFiles) 
    jobFilesTable$list <- jobFiles
})
addDataListObserver(module, summaryTemplate, jobFilesTable, function(jobFile, id){
    data.frame(
        Suite       = jobFile$suite,
        Pipeline    = jobFile$pipeline,
        FileName    = jobFile$name,
        Directory   = jobFile$directory,
            stringsAsFactors = FALSE
    )
})
observeEvent(jobFilesTable$selected(), {
    selected <- jobFilesTable$selected()
    html(
        id = session$ns("table-titleSuffix"), 
        asis = TRUE, 
        html = if(is.na(selected)) "" else paste0(" - ", jobFilesTable$list[[selected]]$name)
    )
})

#----------------------------------------------------------------------
# set return values
#----------------------------------------------------------------------
jobFilesTable

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
