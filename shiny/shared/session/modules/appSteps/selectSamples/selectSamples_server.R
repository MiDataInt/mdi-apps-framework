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
appStepDir <- getAppStepDir(module, shared = TRUE)
if(serverEnv$IS_DEVELOPER) activateMdiHeaderLinks(
    session,
    envir = environment(),   
    baseDirs = appStepDir
)

#----------------------------------------------------------------------
# define session-level and module-level variables
#----------------------------------------------------------------------

# initialize a single source applied to all samples to mimic sourceFileUpload
sourceId <- "allSamples"
sources <- list(list = list())
sources$list[[sourceId]] <- data.frame(
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
    get(options$selectedSamplesTemplate)
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
if(!file.exists(cacheFile)) get(options$getAvailableSamples)()
invalidateAvailableSamples <- reactiveVal(0)
availableSamples <- reactive({
    invalidateAvailableSamples()
    x <- loadPersistentFile(file = cacheFile, ttl = cacheTtl, silent = TRUE)
    req(x)
    persistentCache[[cacheFile]]$data
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
        remove = function(id){
            ids <- selectedIds[selectedIds != id]
            rowIs <- which(availableSamples()[[options$sampleIdCol]] %in% ids)
            availableSamplesTable$table$selectRows(rowIs)
            setSelectedSamples(rowIs)
        }
    ),
    names = list(
        get = getSampleNames,
        source = id
    )
)
setSelectedSamples <- function(rowIs){
    nRows <- length(rowIs)
    if(nRows > 0){
        # lastSelectedRowI <- rowIs[nRows]
        dt <- availableSamples()[rowIs]
        samples$summary <- cbind(
            Remove = "",
            Name = "",
            dt[, .SD, .SDcols = names(get(options$selectedSamplesTemplate))]
        )   
        samples$list <- lapply(seq_len(nrow(dt)), function(i) dt[i])
        ids <- dt[[options$sampleIdCol]]
        names(samples$list) <- ids
        samples$ids <- ids
        selectedIds <<- ids
    } else {
        samples$summary <- sampleSummaryTemplate
        samples$list <- list()
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
        get(options$getAvailableSamples)()
        invalidateAvailableSamples( invalidateAvailableSamples() + 1 )
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
    selection = "multiple",
    selectionFn = setSelectedSamples,
    options = list(
        searchDelay = 0
    ),
    filterable = TRUE
)

#----------------------------------------------------------------------
# define bookmarking actions
#----------------------------------------------------------------------
observe({
    bm <- getModuleBookmark(id, module, bookmark, locks)
    req(bm)
    updateTextInput(session, 'analysisSetName', value = bm$outcomes$analysisSetName)
    samples$list  <- bm$outcomes$samplesList
    samples$names <- bm$outcomes$sampleNames
})

#----------------------------------------------------------------------
# set return values as reactives that will be assigned to app$data[[stepName]]
#----------------------------------------------------------------------
list(
    outcomes = list(
        analysisSetName = reactive(input$analysisSetName),
        sources         = reactive(sources$list),
        samplesList     = reactive(samples$list),
        samples = reactive({
            data.table(
                Source_ID = sourceId,
                Project = sourceId,
                Sample_ID = selectedIds,

                Description = selectedIds ###############

            )
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
