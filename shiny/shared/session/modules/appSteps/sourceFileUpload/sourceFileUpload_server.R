
#----------------------------------------------------------------------
# reactive components for the UI to upload additional sample data files and rename samples
# this is typically the first module of all apps
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
sourceFileUploadServer <- function(id, options, bookmark, locks) { 
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        module <- 'sourceFileUpload' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# define session-level and module-level variables
#----------------------------------------------------------------------
sourceFileInput  <- sourceFileInputServer('fileInput', appName=app$info$name)
cft <- CONSTANTS$contentFileTypes
manifestFileType <- cft$manifestFile
qcReportFileType <- cft$qcReport
statusFileType   <- cft$statusFile

# initialize project (parent table)
sourceSummaryTemplate <- data.frame(
    Remove      = character(),
    FileName    = character(),    
    Project     = character(),
    N_Samples   = integer(),
    Avg_Yield   = integer(),
    Avg_Quality = numeric(),
    QC_Report   = character(),
        stringsAsFactors = FALSE
)
sources <- summaryTableServer(
    id = 'sources', # NOT ns(id) when nesting modules!
    parentId = id,
    stepNumber = options$stepNumber,
    stepLocks = locks[[id]],
    sendFeedback = sourceFileInput$sendFeedback,
    template = sourceSummaryTemplate,
    type = 'shortList',
    remove = list(
        message = "Remove this sample source and all of its samples?",
        name = 'fileName'
    )
)

# initialize samples (child table)
sampleTableTemplate <- data.frame(
    Source_ID   = character(),
    Project     = character(),
    Sample_ID   = character(),
    Description = character(),
    Yield       = integer(),
    Quality     = numeric(),    
        stringsAsFactors = FALSE
)
sampleSummaryTemplate <- data.frame(
    Name         = character(),
    Project      = character(), # first three inherited from project
    Sample_ID    = character(),
    Description  = character(),
        stringsAsFactors = FALSE
)
samples <- summaryTableServer(
    id = 'samples', # NOT ns(id) when nesting modules!
    parentId = id,
    stepNumber = options$stepNumber,
    stepLocks = locks[[id]],
    sendFeedback = sourceFileInput$sendFeedback,
    template = sampleSummaryTemplate,
    type = 'longList100',
    names = list(
        get = getSampleNames,
        source = id
    ),
    parent = list( # enable filtering based on selected sample source 
        keyColumn = "Source_ID",
        table = sources
    )
)

#----------------------------------------------------------------------
# load an incoming data source file (either via launch page or app step 1)
#----------------------------------------------------------------------
loadSourceFile <- function(incomingFile){
    reportProgress('loadSourceFile', module)
    reportProgress(incomingFile$path, module)
    startSpinner(session, 'loadSourceFile')
    sourceId <- tools::md5sum(incomingFile$path) # treat md5 sums as "effectively unique" identifiers
    sourceType <- incomingFile$type
    sft <- CONSTANTS$sourceFileTypes
    loaded <- if(sourceType == sft$project)   loadProjectFile (incomingFile$path, sourceId)
         else if(sourceType == sft$manifest)  loadManifestFile(incomingFile$path, sourceId)
         else if(sourceType == sft$dataTable) loadDataTable   (incomingFile$path, sourceId)
    unlink(incomingFile$path)         
    sources$list[[sourceId]] <- c(
        loaded,
        list(
            sourceType = sourceType,
            fileName = incomingFile$name
        )
    )
    stopSpinner(session, 'loadSourceFile')
    sourceFileInput$sendFeedback(paste(loaded$nSamples, "sample(s) loaded"))    
}
# validate and merge an _additional_ source data file being uploaded by user via step 1 (not launch page)
observeEvent(sourceFileInput$file(), {
    x <- sourceFileInput$file()
    req(x)
    type <- x$type
    req(type)
    loadSourceFile(x)
})
badSourceFile <- function(filePath, msg=""){
    unlink(filePath)
    stopSpinner(session, '!!!! badSourceFile !!!!')    
    sourceFileInput$sendFeedback(paste("bad source file:", msg), isError=TRUE)    
}

#----------------------------------------------------------------------
# load an incoming pipeline output project file
#----------------------------------------------------------------------
loadProjectFile <- function(projectPath, projectId){ # projectPath already validated upstream as a project file usable by app
    dataDir <- getProjectDir(projectId)

    # extract the contents declared to be in the project file
    projectConfig <- getProjectFileConfig(projectPath, sourceFileInput$sendFeedback)
    if(is.null(projectConfig$uploadType)) badSourceFile(projectPath, msg="missing upload type in project file")
    contentFileTypes <- app$info$uploadTypes[[ projectConfig$uploadType ]]$contentFileTypes
    contentFileTypes[[manifestFileType]] <- list(required = TRUE)
    contentFileTypeNames <- names(contentFileTypes)
    projectFileTypeNames <- names(projectConfig$files)

    # check that all required project files are present (before extracting anything)
    for(contentFileTypeName in contentFileTypeNames){
        x <- contentFileTypes[[contentFileTypeName]]
        if(is.null(x$required) || !x$required) next # don't worry about optional files
        matchCount <- sum(contentFileTypeName == projectFileTypeNames)
        if(matchCount == 0) badSourceFile(projectPath, paste("missing file type in project:", contentFileTypeName))
    }

    # extract the files required by, or compatible with, the app
    unzip(projectPath, files='package.yml', exdir=dataDir)        
    for(contentFileTypeName in unique(c(contentFileTypeNames, statusFileType, qcReportFileType))){
        projectFile <- projectConfig$files[[contentFileTypeName]]
        if(is.null(projectFile)) next
        fileName <- projectFile$file
        tryCatch({
            unzip(projectPath, files=fileName, exdir=dataDir)        
        }, error=function(e) badSourceFile(projectPath, paste("could not extract file from project:", fileName)) )
    } 

    # load the manifest file
    manifestFile <- projectConfig$files[[manifestFileType]]
    manifestPath <- getProjectFileByName(projectId, manifestFile$file)
    manifestType <- if(is.null(manifestFile$manifestType)) 'IlluminaDefault' else manifestFile$manifestType # TODO: more intelligent guessing? 
    if(is.null(manifestTypes[[manifestType]])) badSourceFile(projectPath, paste('unknown manifest type in project:', manifestType) )
    manifest <- parseManifestFile(manifestPath, manifestType, projectPath)
    
    # return our results
    c(
        manifest,
        list(
            dataDir  = dataDir,
            config = projectConfig 
        )
    )
}

#----------------------------------------------------------------------
# load an incoming sequencing or other project sample manifest
#----------------------------------------------------------------------
loadManifestFile <- function(manifestPath, manifestId){
    manifestType <- 'IlluminaDefault' # TODO: smarter manifest type ID
    manifest <- parseManifestFile(manifestPath, manifestType)
    c(
        manifest,
        list(
            config = {} 
        )
    )
}
parseManifestFile <- function(manifestPath, manifestType, errorPath=NULL){
    manifest <- tryCatch({
        x <- manifestTypes[[manifestType]]$load(manifestPath)
        manifestTypes[[manifestType]]$parse(x)
    }, error = function(e){
        print(e)
        if(is.null(errorPath)) errorPath <- manifestPath
        badSourceFile(errorPath, "could not parse manifest file")
    })
    for(col in c('Yield','Quality')) if(is.null(manifest$unique[[col]])) manifest$unique[[col]] <- NA
    list(
        manifestType = manifestType,        
        nSamples = nrow(manifest$unique),
        manifest = manifest$manifest,
        unique   = manifest$unique       
    )
}

#----------------------------------------------------------------------
# load an incoming user-constructed data table
#----------------------------------------------------------------------
loadDataTable <- function(dataTablePath, dataTableId){
    badSourceFile(dataTablePath, "data table loading not implemented yet")
}

#----------------------------------------------------------------------
# reactively update the aggregated projects and samples tables
#   the reponse to user action on archetypal pattern inputs
#----------------------------------------------------------------------
observe({
    reportProgress('observe sources$list', module)
    rs <- sourceSummaryTemplate
    ss <- sampleSummaryTemplate
    st <- sampleTableTemplate
    
    # fill the two tables by project
    nSources <- length(sources$list)
    if(nSources > 0){   
        for(i in 1:nSources){ # whenever the active projects change
            sourceId <- names(sources$list)[i]
            reportProgress(sourceId)    
            source <- sources$list[[sourceId]]

            # assume only one project per manifest file
            qcReport <- source$config$files[[qcReportFileType]]
            qcReport <- if(!is.null(qcReport)) qcReport$file
            rs <- rbind(rs, data.frame(
                Remove      = "",
                FileName    = source$fileName,
                Project     = source$unique[1,'Project'], 
                N_Samples   = source$nSamples,
                Avg_Yield   = round(mean(source$unique$Yield),   0),
                Avq_Quality = round(mean(source$unique$Quality), 1),
                QC_Report   = tableCellActionLinks(ns(qcReportParentId), i, qcReport),
                    stringsAsFactors = FALSE
            ))
    
            # save samples twice, once for UI, once for sharing with other modules
            for(i in 1:nrow(source$unique)){
                sample <- source$unique[i,]
                ss <- rbind(ss, data.frame(
                    Name         = "",
                    Project      = sample$Project,
                    Sample_ID    = sample$Sample_ID,
                    Description  = sample$Description,
                    Yield        = sample$Yield,
                    Quality      = sample$Quality,
                        stringsAsFactors = FALSE
                ))
                st <- rbind(st, data.frame(
                    Source_ID  = sourceId,
                    Project     = sample$Project,
                    Sample_ID   = sample$Sample_ID,
                    Description = sample$Description,
                        stringsAsFactors = FALSE
                ))
            }
        }    
    }

    # update the UI reactives
    sources$summary <- rs
    samples$summary <- ss
    samples$list    <- st
    isolate({
        sources$ids <- names(sources$list)
        samples$ids <- apply(samples$list[,c('Project','Sample_ID')], 1, paste, collapse=":") 
    })
})

#----------------------------------------------------------------------
# show QC reports when available
#----------------------------------------------------------------------
qcReportParentId <- 'showQCReport'
observeEvent(input[[qcReportParentId]], {
    
    # get the target file
    startSpinner(session, 'input[[qcReportParentId]]')    
    reportProgress('input[[qcReportParentId]]', module)
    ij <- getTableActionLinkRowAndItem(input, qcReportParentId)
    source <- sources$list[[ ij[1] ]] # only project files have associated QC reports
    qcFile <- getProjectFileByType(source, qcReportFileType)

    # load into a large modal
    showHtmlModal(
        file  = qcFile$path,
        type  = qcReportFileType,
        title = paste(source$fileName, source$unique[1,'Project'], sep=' / ')
    )
})

#----------------------------------------------------------------------
# define bookmarking actions
#----------------------------------------------------------------------
observe({
    bm <- getModuleBookmark(id, module, bookmark, locks)
    req(bm)
    updateTextInput(session, 'analysisSetName', value = bm$outcomes$analysisSetName)
    sources$list  <- bm$outcomes$sources
    samples$list  <- bm$outcomes$samples
    samples$names <- bm$outcomes$sampleNames
})

#----------------------------------------------------------------------
# set return values as reactives that will be assigned to app$data[[stepName]]
#----------------------------------------------------------------------
list(
    outcomes = list(
        analysisSetName = reactive(input$analysisSetName),
        sources         = reactive(sources$list),
        samples         = reactive(samples$list), # actually a data.frame
        sampleNames     = reactive(samples$names)        
    ),
    loadSourceFile = loadSourceFile,
    isReady = reactive({ getStepReadiness(options$source, samples$list) })
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------

