
#----------------------------------------------------------------------
# reactive components for a shell module for viewing analyis results
# the module is generic and can support any type of completed analysis
#----------------------------------------------------------------------
# module follows the archetype of upper form + lower contents set
#   here, the upper form is simply an analysis schema selection table
#   output is more complex, variable and specified by analysis types
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
viewResultsServer <- function(id, options, bookmark, locks) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        module <- 'viewResults' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize module
#----------------------------------------------------------------------
currentSchemaId <- reactiveVal(NULL)
settings <- stepSettingsServer( # display settings not stored in the UI, exposed by gear icon click
    id = 'settings',
    parentId = id,
    cacheKey = currentSchemaId
)
schema <- selectAnalysesServer(
    id = 'schema',
    parentId = id,
    parentOptions = options
)
job <- reactiveVal(NULL) # analysis output loaded from disk

#----------------------------------------------------------------------
# initialize viewers for each possible analysis type
#----------------------------------------------------------------------
analysisTypeTrigger <- 'viewResults-analysisType'
output$viewerPanels <- renderUI({ # UI
    analysisTypeNames <- getAnalysisTypeNames()
    req(analysisTypeNames)
    lapply(analysisTypeNames, function(analysisTypeName){
        hints <- getAnalysisType(analysisTypeName)$hints
        conditionalPanel(
            paste0("window['", analysisTypeTrigger, "'] === '", analysisTypeName, "'"),
            tagList(
                if(is.null(hints)) tags$div() else tags$ul( lapply(hints, function(x) tags$li(HTML(x)) ) ),
                get(paste0(analysisTypeName, 'UI'))(ns(analysisTypeName), options)                
            )
        )
    })
})
observeEvent(getAnalysisTypeNames(), { # server
    analysisTypes <- getAnalysisTypeNames()
    req(analysisTypes)
    for(analysisType in analysisTypes){
        get(paste0(analysisType, 'Server'))(
            id = analysisType,
            parentId = id,
            options = options,
            job = job,
            settings = settings
        )
    }    
})

#----------------------------------------------------------------------
# reactively load a job's results as user selects a completed analysis
#----------------------------------------------------------------------
setViewerType <- function(analysisType){
    session$sendCustomMessage('updateTrigger', list(
        name  = analysisTypeTrigger,
        value = analysisType)
    )        
}
setAnalysisSettings <- function(template){
    if(is.null(template)) template <- list()
    settings$replace(
        init = NULL, # is read from currentSchemaId
        newTemplate = template       
    ) 
}
clearSelectedAnalysis <- function(){
    job(NULL)
    setViewerType('')        
    currentSchemaId(NULL)
    setAnalysisSettings(list()) 
}
observeEvent(schema$selected(), {
    selected <- schema$selected() # numeric index

    # clear viewer on deselect, or selection of an incomplete or failed analysis
    if(is.na(selected)) return( clearSelectedAnalysis() )
    x <- schema$list[[selected]] # only act on successful analyses
    if(x$status != CONSTANTS$jobStatuses$success$value) return( clearSelectedAnalysis() )    

    # load analysis results (potentially slow, use spinner)
    startSpinner(session, module)    
    id <- names(schema$list)[selected]
    analysisType <- getAnalysisType(x$Analysis_Type)
    job <- structure(
        list(
            schema     = x,
            schemaId   = id
        ),
        class = x$Analysis_Type
    )
    job( loadJobOutput(job) )
    setViewerType(x$Analysis_Type)
    currentSchemaId(id)
    setAnalysisSettings(analysisType$settings)
    stopSpinner(session, module)
})

#----------------------------------------------------------------------
# define bookmarking actions
#----------------------------------------------------------------------
observe({
    bm <- getModuleBookmark(id, module, bookmark, locks)
    req(bm)
    
})

#----------------------------------------------------------------------
# set return value
#----------------------------------------------------------------------
list({
    output = list(
        
        
    )
})

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------

