#----------------------------------------------------------------------
# server components for the assemblyPlots appStep module
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
assemblyPlotsServer <- function(id, options, bookmark, locks) { 
    moduleServer(id, function(input, output, session) {    
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize module and settings
#----------------------------------------------------------------------
module <- 'assemblyPlots'
appStepDir <- getAppStepDir(module)
APC <- CONSTANTS$assemblyPlots
options <- setDefaultOptions(options, stepModuleInfo[[module]])
assemblyOptions <- getAssemblyTypeOptions(options)
assemblyOptions$internalUseSampleColumns <- c("project", "sample", "sample_id", "sampleKey", assemblyOptions$internalUseSampleColumns)
#----------------------------------------------------------------------
settings <- activateMdiHeaderLinks( # uncomment as needed
    session,
    # url = getDocumentationUrl("path/to/docs/README", domain = "xxx"), # for documentation
    # dir = appStepDir, # for terminal emulator
    envir = environment(), # for R console
    baseDirs = appStepDir, # for code viewer/editor
    settings = id, # for step-level settings
    templates = list(
        id,
        assemblyOptions$settings
    )
    # immediate = TRUE # plus any other arguments passed to settingsServer()
)
mergeDoses  <- reactive({ isTruthy(settings$get("Assembly","Merge_Doses")) })
mergeClones <- reactive({ isTruthy(settings$get("Assembly","Merge_Clones")) })
#----------------------------------------------------------------------
x <- "assemblyCache"
if(!exists(x, envir = sessionEnv)) assign(x, new_dataCache(x), envir = sessionEnv)
x <- "loadingAssemblyPlotSet"
if(!exists(x, envir = sessionEnv)) assign(x, NULL,             envir = sessionEnv)

#----------------------------------------------------------------------
# toggle data processing to allow manipulation of selections without too-frequent updates
#----------------------------------------------------------------------
isProcessingData <- reactive( !is.null(input$processingIsSuspended) && 
                                      !input$processingIsSuspended )
updataDataProcessing <- function(processingIsSuspended){
    updateButton(
        session, 
        session$ns("processingIsSuspended"), 
        label = if(processingIsSuspended) "Allow Data Processing" else "Suspend Data Processing",
        style = if(processingIsSuspended) "success" else "danger",
        value = processingIsSuspended
    )
}  
observeEvent(isProcessingData(), {
    updataDataProcessing(!isProcessingData())
})

#----------------------------------------------------------------------
# saving plot sets
#----------------------------------------------------------------------
workingId <- NULL # set to a plot id when editing a previously saved set
sendFeedback <- function(x, ...) output$savePlotSetFeedback <- renderText(x)
getPlotSetName <- function(id){
    name <- savedPlotSets$names[[id]] # user name overrides
    if(is.null(name)) savedPlotSets$list[[id]]$Name else name
}
getPlotSetNames <- function(rows = TRUE){
    sapply(names(savedPlotSets$list)[rows], getPlotSetName)
}
savedPlotSetsTemplate <- data.table(
    Remove      = character(),
    Name        = character(),
    Source      = character(),
    Group_By    = character(),
    Required    = character(),
    Prohibited  = character(),
    Data_Types  = character(),
    Projects    = character()
)
savedPlotSets <- summaryTableServer(
    id = 'savedPlotSets', # NOT ns(id) when nesting modules!
    parentId = id,
    stepNumber = options$stepNumber,
    stepLocks = locks[[id]],
    sendFeedback = sendFeedback,
    template = savedPlotSetsTemplate,
    type = 'shortList',
    remove = list(
        message = "Remove this set of saved plots?",
        name = getPlotSetName
    ),
    names = list(
        get = getPlotSetNames,
        source = id
    )
) 
observeEvent(input$savePlotSet, {
    sourceId <- sourceId()
    req(sourceId, input$savePlotSet, input$savePlotSet != 0)
    d <- list( # plot-defining metadata, shown on Saved Plots table; these define the data available to the plot
        Name = paste("Plot #", length(savedPlotSets$list) + 1),
        Source = getSourceFilePackageName(sourceId), # use the source name, not its unique ID, to allow sample additions to saved plots
        Group_By     = input$Group_By,
        Required     = input$Required,
        Prohibited   = input$Prohibited,
        Data_Types   = input$Data_Types,
        Projects     = input$Projects
    )
    r <- initializeRecordEdit(d, workingId, savedPlotSets$list, 'Plot Set', 'plot set', sendFeedback)
    d <- c(d, sapply(assemblyPlots, function(assemblyPlot){ # non-definining formatting attributes saved with plot but not displayed on Saved Plots table
        list(
            conditions = assemblyPlot$conditionsReactive(), 
            groups     = assemblyPlot$groupsReactive(),
            settings   = assemblyPlot$plot$settings$all_() 
        )
    }, simplify = FALSE, USE.NAMES = TRUE))
    saveEditedRecord(d, workingId, savedPlotSets, r)
    workingId <<- NULL
})
addDataListObserver(module, savedPlotSetsTemplate, savedPlotSets, function(r, id){
    dt <- data.table(
        Remove = '', 
        Name   = '',
        Source = strsplit(r$Source, "\\.")[[1]][1]
    )
    for(x in c("Group_By","Required","Prohibited","Data_Types","Projects")) dt[[x]] <- paste(r[[x]], collapse = "<br>")
    dt
})
observeEvent(savedPlotSets$selected(), {
    plotSetI <- savedPlotSets$selected()
    clearloadingAssemblyPlotSet <- function(...){
        loadingAssemblyPlotSet <<- NULL
        stopSpinner(session)
    } 
    abortSavedPlotSet <- function(...){
        sourceIdOverride(NA)
        workingId <<- NULL
        clearloadingAssemblyPlotSet()
    } 
    if(isTruthy(plotSetI)){
        plotSet <- savedPlotSets$list[[plotSetI]]
        sources <- app$upload$outcomes$sources()
        sourceI <- which(sapply(sources, function(source) source$manifest$Project == plotSet$Source))[1]
        if(isTruthy(sourceI)){
            startSpinner(session, message = "loading saved plots")
            sourceIdOverride(NA) 
            updataDataProcessing(processingIsSuspended = TRUE)
            setTimeout(function(....){ # let the prior plots clear first
                loadingAssemblyPlotSet <<- plotSet
                # waitFor(svFrequenciesPlotTrigger, clearloadingAssemblyPlotSet, delay = 100)
                for(assemblyPlot in assemblyPlots){
                    if(isTruthy(plotSet[[assemblyPlot$id]])){
                        assemblyPlot$plot$settings$replace(plotSet[[assemblyPlot$id]]$settings)
                    }
                }
                sourceIdOverride(names(sources)[sourceI]) # thus, look up the current source of the saved name, which may be updated from the original save
                # setTimeout(clearloadingAssemblyPlotSet, delay = 5000) # in case svFrequenciesPlotTrigger never fires due to failed plot
            }, delay = 100)
        } else abortSavedPlotSet()
    } else abortSavedPlotSet()
})

#----------------------------------------------------------------------
# assembly selection and loading
#----------------------------------------------------------------------
sourceIdOverride <- reactiveVal(NULL)
sourceId <- dataSourceTableServer(
    "dataSource", 
    selection = "single",
    sourceIdOverride = sourceIdOverride
)
assembly <- reactive({
    sourceId <- sourceId()
    req(sourceId)
    rdsFile <- getSourceFilePath(sourceId, "assembly")
    req(file.exists(rdsFile))
    settings_ <- settings$all_()
    settings$Assembly_Plots <- NULL # used by us, not relevant to keying assembly loads
    assembly <- doAssemblyAction("loadAssembly", options, assemblyOptions, settings, rdsFile)
    req(assembly$samples)
    cols <- names(assembly$samples)
    req("sample_id"  %in% cols || "sample" %in% cols)
    if(!("project"   %in% cols)) assembly$samples[, project := getAssemblyPackageName(sourceId)]
    if(!("sample"    %in% cols))  assembly$samples[, sample := "sample_id"]
    if(!("sample_id" %in% cols)) assembly$samples[, sample_id := "sample"]
    if(!("sampleKey" %in% cols)) assembly$samples[, sampleKey := paste(project, sample_id, sep = "::")]
    assembly
})

#----------------------------------------------------------------------
# selection of columns and values to group and plot, applies to all plots
#----------------------------------------------------------------------
groupableColumns <- reactive({
    cols <- names(assembly()$samples)
    cols[!(cols %in% c(
        names(assemblyOptions$dataTypeColumns),
        assemblyOptions$internalUseSampleColumns
    ))]
})
updateColumnSelectors <- function(choices, selected = NULL){
    default <- list(
        Group_By    = character(), 
        Required    = character(), 
        Prohibited  = character(),
        Data_Types  = names(assemblyOptions$dataTypeColumns)[unlist(assemblyOptions$dataTypeColumns)]
    )
    if(is.null(selected)) selected <- default
    for(x in names(default)){
        choices_ <- switch(x,
            Data_Types = names(assemblyOptions$dataTypeColumns),
            choices
        )
        updateCheckboxGroupInput(
            session = session, 
            inputId = x,
            choices = choices_,
            selected = selected[[x]][selected[[x]] %in% choices_],
            inline = TRUE
        )
    } 
}
observeEvent(groupableColumns(), { updateColumnSelectors(groupableColumns(), loadingAssemblyPlotSet) })

#----------------------------------------------------------------------
# optional table of all samples, collapsed on page load
#----------------------------------------------------------------------
allSamplesTable <- bufferedTableServer(
    "allSamples",
    id,
    input,
    reactive({ 
        samples <- assembly()$samples
        groupableColumns <- groupableColumns()
        req(samples, groupableColumns)
        cols <- unique(c(
            "project", "sample", 
            groupableColumns,
            assemblyOptions$showSampleColumns,
            names(assemblyOptions$dataTypeColumns)
        ))
        cols <- cols[cols %in% names(samples)]
        samples[, .SD, .SDcols = cols] 
    }),
    selection = 'none',
    options = list(),
    filterable = TRUE
)

#----------------------------------------------------------------------
# working table of all samples matching grouping filters (not displayed to user)
#----------------------------------------------------------------------
groupedSamples <- reactive({
    samples <- assembly()$samples
    groupableColumns <- groupableColumns()
    req(samples, groupableColumns)
    groupedColumns <- input$Group_By
    if(length(groupedColumns) == 0) groupedColumns = groupableColumns
    ungroupedColumns <- groupableColumns[!(groupableColumns %in% groupedColumns)]
    assemblyCache$get(
        'groupedSamples', 
        permanent = TRUE,
        from = "ram",
        create = assemblyOptions$cacheCreateLevel, # 'asNeeded', 'once', 'always'
        keyObject = list(
            samples = samples,
            groupableColumns = groupableColumns,
            groupedColumns = groupedColumns,
            ungroupedColumns = ungroupedColumns,
            required = input$Required,
            prohibited = input$Prohibited,
            Data_Types = input$Data_Types
        ), 
        createFn = function(...) {
            startSpinner(session, message = "getting samples")
            for(column in unique(c(ungroupedColumns, input$Prohibited))){
                I <- samples[[column]] == "-" # all ungrouped/prohibited columns must be "-"
                samples <- samples[I]
            }
            for(column in input$Required){
                I <- samples[[column]] == "-" # all required columns must not be "-"
                samples <- samples[!I]
            }
            isVariable <- sapply(groupedColumns, function(column) length(unique(samples[[column]])) > 1) # only report informative columns
            variableColumns <- groupedColumns[isVariable]
            x <- samples[, .SD, .SDcols = c("project", "sample", 
                                            variableColumns, 
                                            assemblyOptions$showSampleColumns, 
                                            input$Data_Types)]
            if(mergeDoses())  for(column in variableColumns) x[[column]] <- assemblyDosesToLogical(x, column)
            if(mergeClones()) for(column in variableColumns) x[[column]] <- assemblyClonesToTargets(x, column)
            stopSpinner(session)
            x
        }
    )$value
})

# ----------------------------------------------------------------------
# selection of projects to group and plot; allows quick dropping of all samples in a project
# ----------------------------------------------------------------------
updateProjectSelector <- function(selected = NULL){
    groupedSamples <- groupedSamples()
    req(groupedSamples)
    projects <- unique(groupedSamples$project) # by default, all projects are selected
    updateCheckboxGroupInput(
        session  = session,
        inputId  = "Projects",
        choices  = projects,
        selected = if(is.null(selected)) projects else selected$Projects,
        inline = TRUE
    )
}
observeEvent(groupedSamples(), { updateProjectSelector(loadingAssemblyPlotSet) })

# ----------------------------------------------------------------------
# table of all samples after applying grouping and project filters
# ----------------------------------------------------------------------
groupedProjectSamples <- reactive({ # not a slow step, not worth caching
    groupedSamples <- groupedSamples()
    req(groupedSamples)
    startSpinner(session, message = "getting project samples")
    x <- groupedSamples[project %in% input$Projects]
    stopSpinner(session)
    x
})
groupedProjectSamplesTable <- bufferedTableServer( # called Matching Samples on screen, starts collapsed
    "groupedProjectSamples",
    id,
    input,
    groupedProjectSamples,
    selection = 'none',
    options = list()
)

# ----------------------------------------------------------------------
# aggregate groups automatically determined from groupedProjectSamples
# ----------------------------------------------------------------------
groupingCols <- reactive({
    cols <- names(groupedProjectSamples())
    cols[cols %in% groupableColumns()] 
})
groups <- reactive({
    req(isProcessingData())
    groupedProjectSamples <- groupedProjectSamples()
    groupingCols <- groupingCols()
    req(groupedProjectSamples)
    doAssemblyAction("getGroups", options, assemblyOptions,
                     groupedProjectSamples, groupingCols, input)
})
groupsTable <- bufferedTableServer(
    "groups",
    id,
    input,
    reactive({ 
        groups <- groups() 
        groupingCols <- groupingCols() 
        cols <- names(groups)
        groups[, .SD, .SDcols = cols[cols %in% c(
            groupingCols, 
            "nProjects", "nSamples",
            assemblyOptions$showGroupColumns
        )]]
    }),
    selection = 'none',
    options = list(
        paging = FALSE,
        searching = FALSE
    )
)

# ----------------------------------------------------------------------
# output plots, each with its own set of group and condition sortables
# ----------------------------------------------------------------------
assemblyPlots <- sapply(names(assemblyOptions$plotTypes), function(id){
    doAssemblyAction(
        paste0(id, "Server"), options,
        id, session, input, output, 
        isProcessingData, assemblyOptions,
        sourceId, assembly, groupedProjectSamples, groupingCols, groups
    )
}, simplify = FALSE, USE.NAMES = TRUE)

#----------------------------------------------------------------------
# define bookmarking actions
#----------------------------------------------------------------------
selfDestruct <- observe({
    bm <- getModuleBookmark(id, module, bookmark, locks)
    req(bm)
    settings$replace(bm$settings)
    savedPlotSets$list  <- bm$outcomes$plotSets
    savedPlotSets$names <- bm$outcomes$plotSetNames
    selfDestruct$destroy()
})

#----------------------------------------------------------------------
# set return values as reactives that will be assigned to app$data[[stepName]]
#----------------------------------------------------------------------
list(
    input = input,
    settings = settings$all_,
    outcomes = list(
        plotSets     = reactive(savedPlotSets$list),
        plotSetNames = reactive(savedPlotSets$names)
    ),
    # isReady = reactive({ getStepReadiness(options$source, ...) }),
    NULL
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
