#----------------------------------------------------------------------
# static components to select samples from a single list of all samples
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
selectSamplesServer <- function(id, options, bookmark, locks) { 
    moduleServer(id, function(input, output, session) {    
#----------------------------------------------------------------------
module <- 'selectSamples'
options <- setDefaultOptions(options, stepModuleInfo[[module]])
appStepDir <- getAppStepDir(module)
if(serverEnv$IS_DEVELOPER) activateMdiHeaderLinks(
    session,
    url = getDocumentationUrl("shiny/shared/session/modules/appSteps/selectSamples/README", 
                              framework = TRUE),
    envir = environment(),   
    baseDirs = appStepDir
)

#----------------------------------------------------------------------
# define session-level and module-level variables
#----------------------------------------------------------------------

# initialize a single source applied to all samples to mimic sourceFileUpload
sourceId <- "allSamples"
sources <- list(list = list())
sources$list[[sourceId]] <- data.table(
    FileName    = "",    
    Project     = sourceId,
    N_Samples   = 0,
    Avg_Yield   = 0,
    Avg_Quality = 0,
    QC_Report   = ""          
)
sources$summary <- sources$list[[sourceId]]

# initialize samples tables
sampleSummaryTemplate <- cbind(
    Remove = character(),
    Name = character(),
    as.data.table(get(options$selectedSamplesTemplate))
)

#----------------------------------------------------------------------
# load an incoming data source file (either via launch page or app step 1)
#----------------------------------------------------------------------
loadSourceFile <- function(incomingFile, suppressUnlink = FALSE){
    if(is.null(incomingFile) || length(incomingFile$name) == 0) return(NULL)
    NULL # selectSamples only supports cold start and bookmarks 
}

#----------------------------------------------------------------------
# create and update a server cache of all available Bru-seq samples
#----------------------------------------------------------------------
cacheFileName <- paste(app$NAME, "availableSamples", sep = ".")
cacheFile <- file.path(serverEnv$CACHE_DIR, paste(cacheFileName, "rds", sep = "."))
cacheTtl <- 7 * 24 * 60 * 60 # TODO: expose as option?
if(!file.exists(cacheFile)) get(options$cacheAvailableSamples)(cacheFile)
invalidateAvailableSamples <- reactiveVal(list(entropy = sample(1e8, 1), force = FALSE))
availableSamples <- reactive({
    action <- invalidateAvailableSamples()
    req(action)
    x <- loadPersistentFile(file = cacheFile, ttl = cacheTtl, silent = TRUE, force = action$force)
    req(x)
    as.data.table(persistentCache[[cacheFile]]$data)
})

#----------------------------------------------------------------------
# table of all selected samples with remove buttons and name edit boxes
#----------------------------------------------------------------------
selectedIds <- character()
samples <- summaryTableServer(
    id = 'selectedSamples',
    parentId = id,
    stepNumber = options$stepNumber,
    stepLocks = locks[[id]],
    sendFeedback = function(...) NULL,
    template = sampleSummaryTemplate,
    type = 'longList100',
    remove = list(
        confirm = FALSE,
        remove = function(id) {
            samples$names <- samples$names[names(samples$names) != paste(sourceId, id, sep = ":")]
            setSelectedSamplesById(selectedIds[selectedIds != id])
        }
    ),
    names = list(
        get = getSampleNames,
        source = id
    )
)
samples$table <- NULL # custom addition to samples object create by us, for filling outcomes

#----------------------------------------------------------------------
# execute sample selections (i.e., link selected samples table to available samples table)
#---------------------------------------------------------------------
setSelectedSamplesById <- function(ids){
    rowIs <- which(availableSamples()[[options$sampleIdCol]] %in% ids)
    availableSamplesTable$table$selectRows(NULL)
    setSelectedSamples(rowIs)    
}
setSelectedSamples <- function(rowIs){
    nRows <- length(rowIs)
    if(nRows > 0 && !is.na(rowIs[1])){
        dt <- availableSamples()[rowIs]
        samples$table <- dt
        samples$summary <- cbind(
            Remove = "",
            Name = "",
            dt[, .SD, .SDcols = names(get(options$selectedSamplesTemplate))]
        )   
        samples$list <- lapply(seq_len(nrow(dt)), function(i) dt[i])
        ids <- dt[[options$sampleIdCol]]
        names(samples$list) <- ids
        selectedIds <<- ids
        samples$ids <- paste(sourceId, ids, sep = ":")
    } else {
        samples$table <- NULL
        samples$summary <- sampleSummaryTemplate
        samples$list <- list()
        selectedIds <<- character()        
        samples$ids <- character()
    }
}

#----------------------------------------------------------------------
# table of all available samples for making selections
#----------------------------------------------------------------------
availableSamplesTable <- bufferedTableBoxServer(
    id = "availableSamples",
    #----------------------------
    reload = function(){
        runjs(paste0('$("#', session$ns("availableSamples-reload"), '").blur()'))
        get(options$cacheAvailableSamples)(cacheFile)
        invalidateAvailableSamples( list(entropy = sample(1e8, 1), force = TRUE) )
    },
    download = downloadHandler(
        filename = paste(cacheFileName, "csv", sep = "."),
        content = function(tmpFile) {
            runjs(paste0('$("#', session$ns("availableSamples-download"), '").blur()'))
            write.csv(availableSamples(), tmpFile, row.names = FALSE)
        },
        contentType = "text/csv"
    ),
    #----------------------------
    tableData = availableSamples,
    selection = "single",
    options = list(
        searchDelay = 0
    ),
    filterable = TRUE
)
observeEvent(availableSamplesTable$table$selectionObserver(), {
    rowI <- availableSamplesTable$table$selectionObserver()
    req(rowI)
    setSelectedSamplesById(unique(c(
        selectedIds,
        availableSamples()[rowI, .SD, .SDcols = options$sampleIdCol]
    )))
})

#----------------------------------------------------------------------
# define bookmarking actions
#----------------------------------------------------------------------
observe({
    bm <- getModuleBookmark(id, module, bookmark, locks)
    req(bm)
    updateTextInput(session, 'analysisSetName', value = bm$outcomes$analysisSetName)
    samples$list  <- bm$outcomes$samplesList
    samples$names <- bm$outcomes$sampleNames
    isolate({ 
        setSelectedSamplesById(names(bm$outcomes$samplesList)) 
    })
})

#----------------------------------------------------------------------
# set return values as reactives that will be assigned to app$data[[stepName]]
#----------------------------------------------------------------------
list(
    outcomes = list(
        analysisSetName = reactive(input$analysisSetName),
        sources     = reactive(sources$list),
        samplesList = reactive(samples$list),
        samples = reactive({  
            if(!is.null(samples$table) && nrow(samples$table) > 0){
                data.table(
                    Source_ID   = sourceId,
                    Project     = sourceId,
                    Sample_ID   = samples$table[[options$sampleIdCol]],
                    Description = if(is.null(options$descriptionCol)) "" 
                                else samples$table[[options$descriptionCol]]
                )  
            } else {
                data.table(
                    Source_ID   = character(),
                    Project     = character(),
                    Sample_ID   = character(),
                    Description = character()
                )
            }
        }),
        sampleNames = reactive(samples$names)        
    ),
    sourcesSummary = reactive(sources$summary),
    loadSourceFile = loadSourceFile,
    isReady = reactive({ getStepReadiness(options$source, samples$list) })
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
