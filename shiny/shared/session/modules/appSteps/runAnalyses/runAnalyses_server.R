#----------------------------------------------------------------------
# reactive components used to build executable analyses from sample sets
# the module is generic and can support any analysis type, as requested in options
#----------------------------------------------------------------------
# module follows the archetype of upper edit form + lower summary list
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
runAnalysesServer <- function(id, options, bookmark, locks) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        module <- 'runAnalyses' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize module and the requested analysisTypes
#----------------------------------------------------------------------
assignOutcomes <- getStepOutcomesByType("assign") # depends on sample assignment appStep
universalOptions <- file.path('session','types','analysisTypes','universalAnalysisOptions.yml')
universalOptions <- read_yaml(universalOptions)
universalOptionNames <- names(universalOptions)
analysisTypes <- unlist(lapply(names(options$analysisTypes), function(typeType){
    typeNames <- options$analysisTypes[[typeType]]
    lapply(typeNames, function(typeName){
        dir <- file.path('optional','types','analysisTypes', typeType, typeName)
        loadAllRScripts(dir, recursive=TRUE) # load the analysisType module and methods
        config <- read_yaml(file.path(dir,'config.yml')) # load the analysisType config        
        for(loadType in c('classes','modules')){
            if(is.null(config[[loadType]])) next        
            for(type in names(config[[loadType]])){ 
                for(target in unique(config[[loadType]][[type]])){ # load any modules and classes required by the analysisType
                    loadAllRScripts(file.path('optional', loadType, type, target), recursive=TRUE)
                }        
            }
        }
        if(is.null(config$options)) config$options <- list()
        config
    }) %>% setNames(typeNames)
}), recursive=FALSE) # discard the analysis typeType
analysisTypeNames <- names(analysisTypes)

#----------------------------------------------------------------------
# define session-level and module-level variables
#----------------------------------------------------------------------

# initialize analysis schema
summaryTemplate <- data.frame(
    Remove      = character(),
    Name        = character(),
    Sample_Set  = character(),
    Analysis_Type  = character(),
    Options     = character(),
    Job_Status  = character(),
        stringsAsFactors = FALSE
)
data <- summaryTableServer(
    id = 'schema', # NOT ns(id) when nesting modules!
    parentId = id,
    stepNumber = options$stepNumber,
    stepLocks = locks[[id]],
    sendFeedback = sendFeedback,
    template = summaryTemplate,
    type = 'shortList',
    remove = list(
        message = "Remove this analysis schema and all files from any executed job?",
        name = getSchemaName
    ),
    names = list(
        get = getSchemaNames,
        source = id
    ),
    clearLocks = function(analysisId) changeLocks(analysisId, clearRecordLocks)
)

#----------------------------------------------------------------------
# how to lock and unlock sample sets from analyses
#----------------------------------------------------------------------
changeLocks <- function(analysisId, lockFn){
    sampleSetId <- data$list[[analysisId]]$Sample_Set
    lockFn(locks[[options$source]], module, analysisId, sampleSetId)   
}

#----------------------------------------------------------------------
# helper functions for assembling analysis options
#----------------------------------------------------------------------
analysisOptionInputs <- function(analysisOptions, nColumns){
    req(analysisOptions)
    if(length(analysisOptions) == 0) return(NULL)
    optionValues <- getOptionValuesFromSchema(analysisOptions, editPanelData()$selectedSchema)
    width <- 12 / nColumns
    is <- 1:length(analysisOptions)
    x <- lapply(is, function(i){
        value <- if(is.null(optionValues)) NULL else optionValues[i]
        column(
            width = width,
            getAnalysisOptionInputs(ns, analysisTypeNames, analysisOptions[i], value)
        )
    })
    lapply(split(x, floor((is-1)/4)), function(row) fluidRow(tagList(row)) ) # parse into rows of 4 options each
}

#----------------------------------------------------------------------
# choose input sample(s) and set analysis options via a form
#----------------------------------------------------------------------

# update the edit panel reactively
workingId <- NULL
updateEditPanel <- reactiveVal(0)
editPanelData <- reactiveVal(NULL)
boxTitle <- "Set Analysis Options"
observeEvent({
    assignOutcomes$sampleSets()     # when new sample assignments are available
    assignOutcomes$sampleSetNames() # so that names stay fresh
    updateEditPanel() # when user clicks reset or similar action
    data$selected()   # when user clicks an existing sample set for editing
}, {
    reportProgress('initializeEditPanel', module)
    selected <- data$selected()

    # collect information on the state of the grid
    ep <- if(is.na(selected)){
        workingId <<- NULL
        list(
            selectedSchema = NULL,
            boxTitle = boxTitle
        )
    } else {
        workingId <<- names(data$list)[selected]
        list(
            selectedSchema = data$list[[selected]],
            boxTitle = paste(boxTitle, getSchemaName(workingId), sep=" - ")
        )
    }

    # send results to our dependents
    ep$force <- sample(1e8, 1)
    editPanelData(ep)
})

# title of the form box
output$boxTitle <- renderText({ editPanelData()$boxTitle })

# options that apply to all analysis schema
output$universalOptions <- renderUI({
    reportProgress('output$universalOptions', module)
    #sendFeedback(NULL)    
    analysisOptionInputs(universalOptions, 2)
})

# options that cascade from the selected analysis type
output$specificOptions <- renderUI({
    reportProgress('output$specificOptions', module)
    req(input$Analysis_Type)
    analysisOptions <- analysisTypes[[input$Analysis_Type]]$options
    analysisOptionInputs(analysisOptions, 4)
})

#----------------------------------------------------------------------
# commit the form when user clicks the Save button
#----------------------------------------------------------------------

# customize the save button feedback
sendFeedback <- recordFeedbackFunction(output, 'saveRecordFeedback')

# assemble/edit a record
observeEvent(input$saveRecord, {

    # suppress error message on first load
    req(input$saveRecord)
    if(input$saveRecord == 0) return("")
    reportProgress('output$saveRecord', module)

    # initialize a new analysis object
    if(input$Sample_Set    == "") sendFeedback('missing sample(s) selection', TRUE)
    if(input$Analysis_Type == "") sendFeedback('missing analysis type', TRUE)
    analysisType <- analysisTypes[[input$Analysis_Type]]
    d <- getOptionValuesFromUI(analysisType$options, input, addOptions=universalOptions)

    # create a ~unique identifying signature to prevent record duplicates
    #   defining values:
    #       sample source Id (based on sample(s) UID and group assignments)
    #       job options
    r <- initializeRecordEdit(d, workingId, data$list, 'Analysis', 'analysis schema', sendFeedback)

    # continue filling non-defining record values
    #   set the analysis status
    #   abort record save if analysis is already completed
    d$optionsHtml <- getOptionsUIFromSchema(analysisTypeNames, analysisType$options, d, exclude=universalOptionNames)
    d$name <- r$name
    diskStatus <- getJobDiskStatus(r$id)  
    d$status <- if(!is.null(diskStatus)) diskStatus
        else if(r$isEdit) data$list[[workingId]]$status
        else CONSTANTS$jobStatuses$created$value
    if(r$isEdit && d$status >= CONSTANTS$jobStatuses$running$value)
        sendFeedback('you cannot edit a completed or running analysis', TRUE)      

    # save our work
    saveEditedRecord(d, workingId, data, r)
    
    # place a lock on the parent manifest of all samples in the set
    if(r$isEdit) changeLocks(workingId, clearRecordLocks)
    changeLocks(r$id, placeRecordLocks)        

    # report success
    workingId <<- NULL
    sendFeedback("analysis saved")
})

#----------------------------------------------------------------------
# clear any selected analysis schema table rows on form reset or other relevant change
#----------------------------------------------------------------------
resetEditPanel <- addResetObserver(c(
    'resetEditPanel'
), input, module, data, sendFeedback, updateEditPanel)


#----------------------------------------------------------------------
# reactively update the analysis launcher summary table
#   the reponse to user action on archetypal pattern inputs
#----------------------------------------------------------------------
addDataListObserver(module, summaryTemplate, data, function(schema, id){
    data.frame(
        Remove        = '',
        Name          = '',
        Sample_Set    = getSampleSetName(schema$Sample_Set),
        Analysis_Type = schema$Analysis_Type,
        Options       = schema$optionsHtml,
        Job_Status    = schema$status,
            stringsAsFactors=FALSE
    )
})

#----------------------------------------------------------------------
# define bookmarking actions
#----------------------------------------------------------------------
observe({
    bm <- getModuleBookmark(id, module, bookmark, locks)
    req(bm)
    data$list  <- bm$outcome$schema
    data$names <- bm$outcome$schemaNames 
})

#----------------------------------------------------------------------
# set return values as reactives that will be assigned to app$data[[stepName]]
#----------------------------------------------------------------------
list(
    outcomes = list(
        schema      = reactive(data$list),
        schemaNames = reactive(data$names)        
    ),
    analysisTypes = analysisTypes,
    analysisTypeNames = analysisTypeNames,
    isReady = reactive({ getStepReadiness(options$source, fn=areSuccessfulAnalyses) })
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------

