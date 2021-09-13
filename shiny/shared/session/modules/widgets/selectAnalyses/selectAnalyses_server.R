
#----------------------------------------------------------------------
# reactive components for a table that lists completed analysis jobs
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
selectAnalysesServer <- function(id, parentId, parentOptions, status=NULL) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        parentNS <- NS(parentId)
        module <- 'selectAnalyses' # for reportProgress tracing
#----------------------------------------------------------------------
    
#----------------------------------------------------------------------
# define session-level and module-level variables
#----------------------------------------------------------------------

# initialize analysis schema
summaryTemplate <- data.frame(
    Analysis_Name  = character(),
    Sample_Set     = character(),
    Analysis_Type  = character(),
    Job_Status  = character(),
        stringsAsFactors = FALSE
)
schemaTable <- summaryTableServer(
    id = 'table', # NOT ns(id) when nesting modules!
    parentId = id,
    stepNumber = parentOptions$stepNumber,
    stepLocks = locks[[parentId]],
    sendFeedback = NULL,
    template = summaryTemplate,
    type = 'shortList'
)

#----------------------------------------------------------------------
# reactively update the analysis schema summary table as job statuses change
#----------------------------------------------------------------------
observe({
    schema <- getFilteredAnalyses(status)
    req(schema) 
    schemaTable$list <- schema
})
addDataListObserver(module, summaryTemplate, schemaTable, function(schema, id){
    schema$status[schema$status == CONSTANTS$jobStatuses$created$value] <- NA # mask launch job links here
    data.frame(
        Analysis_Name = getSchemaName(id),
        Sample_Set    = getSampleSetName(schema$Sample_Set),
        Analysis_Type = schema$Analysis_Type,
        Job_Status    = schema$status,
            stringsAsFactors = FALSE
    )
})

#----------------------------------------------------------------------
# set return values
#----------------------------------------------------------------------
schemaTable

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------

