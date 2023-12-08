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
assemblyOptions$internalUseSampleColumns <- c("project", "sample", "sample_id", assemblyOptions$internalUseSampleColumns)
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
isProcessingData <- reactive( !is.null(input$suspendDataProcessing) && 
                                      !input$suspendDataProcessing )
observeEvent(isProcessingData(), {
    isProcessingData <- isProcessingData()
    updateButton(
        session, 
        session$ns("suspendDataProcessing"), 
        label = if(isProcessingData) "Suspend Data Processing" else "Allow Data Processing",
        style = if(isProcessingData) "danger" else "success"
    )
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
    d <- c(d, list( # non-definining formatting attributes saved with plot but not displayed on Saved Plots table
        conditions_svFrequencies = conditions_svFrequencies(), 
        groups_svFrequencies = groups_svFrequencies(),
        settings_svFrequencies = svFrequenciesPlot$settings$all_(),

        conditions_microhomology = conditions_microhomology(), 
        groups_microhomology = groups_microhomology(),
        settings_microhomology = microhomologyPlot$settings$all_(),

        conditions_endpoints = conditions_endpoints(), 
        groups_endpoints = groups_endpoints(),
        settings_endpoints = endpointsPlot$settings$all_(),

        conditions_svSizes = conditions_svSizes(), 
        groups_svSizes = groups_svSizes(),
        settings_svSizes = svSizesPlot$settings$all_()
    ))
    saveEditedRecord(d, workingId, savedPlotSets, r)
    workingId <<- NULL
})
addDataListObserver(module, savedPlotSetsTemplate, savedPlotSets, function(r, id){
    dt <- data.table(
        Remove = '', 
        Name   = '',
        Source = strsplit(r$Source, ".")[[1]][1]
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
            setTimeout(function(....){ # let the prior plot clear first
                loadingAssemblyPlotSet <<- plotSet

                waitFor(svFrequenciesPlotTrigger, clearloadingAssemblyPlotSet, delay = 100)

                svFrequenciesPlot$settings$replace(plotSet$settings_svFrequencies)
                microhomologyPlot$settings$replace(plotSet$settings_microhomology)
                endpointsPlot$settings$replace(plotSet$settings_endpoints)
                svSizesPlot$settings$replace(plotSet$settings_svSizes)

                sourceIdOverride(names(sources)[sourceI]) # thus, look up the current source of the saved name, which may be updated from the original save
                setTimeout(clearloadingAssemblyPlotSet, delay = 5000) # in case svFrequenciesPlotTrigger never fires due to failed plot
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
    doAssemblyAction("loadAssembly", options, assemblyOptions, 
                     settings, rdsFile)
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
    # cols[!(cols %in% c(
    #     "project", "sample", 
    #     "coverage", 
    #     denominatorColumns, 
    #     svTypeColumns
    # ))]    
})
groups <- reactive({
    req(isProcessingData())
    groupedProjectSamples <- groupedProjectSamples()
    groupingCols <- groupingCols()
    req(groupedProjectSamples, groupingCols)
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
for(id in names(assemblyOptions$plotTypes)){
    pt <- assemblyOptions$plotTypes[[id]]
    assemblyPlotBoxServer(
        id, pt$label, collapsed = pt$collapsed, plotBoxUI = pt$plotBoxUI
    )
}

# #----------------------------------------------------------------------
# # microhomology distribution plot
# #----------------------------------------------------------------------
# microhomologyType <- "microhomology"
# conditions_microhomology <- reactiveVal(NULL)
# output$conditions_microhomology <- renderAssemblyConditionsBucket(session, groupingCols, microhomologyType, conditions_microhomology)
# observeEvent(input$conditions_microhomologyOnChange, { conditions_microhomology(input$conditions_microhomologyOnChange) })
# #----------------------------------------------------------------------
# groups_microhomology <- reactiveVal(NULL)
# output$groups_microhomology <- renderAssemblyGroupsBucket(session, groups, microhomologyType, groups_microhomology)
# observeEvent(input$groups_microhomologyOnChange, { groups_microhomology(input$groups_microhomologyOnChange) })
#----------------------------------------------------------------------
# microhomologyData <- reactive({
#     req(isProcessingData())
#     svs <- matchingSvs()    
#     groupingCols <- groupingCols()
#     conditions_ <- conditions_microhomology()
#     groups_ <- groups_microhomology()
#     req(svs, groupingCols, conditions_, groups_)
#     assemblyCache$get(
#         'microhomologyData', 
#         permanent = TRUE,
#         from = "ram",
#         create = assemblyOptions$cacheCreateLevel, # 'asNeeded', 'once', 'always'
#         keyObject = list(
#             svs = svs,
#             groupingCols = groupingCols,
#             conditions_ = conditions_,
#             groups_ = groups_,
#             constants = APC$microhomology,
#             microhomologyPlot$settings$all()
#         ), 
#         createFn = function(...) {
#             startSpinner(session, message = "counting microhomologies")
#             svs <- svs[groupLabels %in% groups_ & N_SPLITS > 0] # omit gap-only junctions
#             svs[, insertSize := -MICROHOM_LEN]
#             titleSuffix <- NULL
#             if(length(conditions_) < length(groupingCols)) {
#                 svs <- setAssemblyGroupLabels(svs, groupingCols, conditions_) # regroup a second time if some conditions were omitted
#                 groups_ <- unique(svs$groupLabels)
#                 titleSuffix <- getDroppedAssemblyGroupLabels(svs, groupingCols, conditions_)
#             }

#             dt <- dcastSvsByGroup(svs, as.formula("insertSize ~ groupLabels"), groups_)
            
#             mar <- APC$microhomology$mar
#             mar[3] <- mar[3] + length(groups_)
#             x <- list(
#                 Plot_Frame = getAssemblyPlotFrame(
#                     microhomologyPlot,
#                     APC$microhomology$insideWidth, 
#                     APC$microhomology$insideHeight, 
#                     mar
#                 ),
#                 mar = mar,
#                 dt = dt,
#                 titleSuffix = titleSuffix,
#                 groupCounts = svs[, .N, by = .(groupLabels)]
#             )
#             stopSpinner(session)
#             x
#         }
#     )$value
# })

#----------------------------------------------------------------------
# microhomologyPlotFrame <- plotFrameReactive(microhomologyData) 
# microhomologyPlot <- mdiDensityPlotBoxServer(
#     "microhomologyPlot",
#     data = reactive({
#         svs <- matchingSvs()   
#         dprint(nrow(svs))    
#         groups_ <- groups_microhomology() 
#         svs <- svs[groupLabels %in% groups_ & N_SPLITS > 0] # omit gap-only junctions
#         svs[, x := -MICROHOM_LEN]
#     }),
#     groupingCols = "groupLabels",
#     "Insert Size (bp)",
#     defaultBinSize = 1,
#     eventTypePlural = "Junctions"
# )

# The data.table provided to mdiDensityPlotBox must have an x columns, plus any columns provided as groupingCols that are used to define data groups.

# staticPlotBoxServer(
#     "microhomologyPlot",
#     settings = assemblyPlotSettings$microhomology, 
#     size = "m",
#     Plot_Frame = microhomologyPlotFrame,
#     create = function() {
#         d <- microhomologyData()
#         req(d, ncol(d$dt) > 1)
#         startSpinner(session, message = "rendering microhomology plot")
#         groupLabels_ <- names(d$dt)[2:ncol(d$dt)]
#         maxY <- d$dt[, max(.SD, na.rm = TRUE), .SDcols = groupLabels_] * 1.05
#         nGroups <- length(groupLabels_)
#         par(mar = d$mar)
#         xlim <- c(
#             microhomologyPlot$settings$get("Limits","Min_X_Value"),
#             microhomologyPlot$settings$get("Limits","Max_X_Value")
#         )
#         microhomologyPlot$initializeFrame(
#             xlim = xlim, # TODO: expose as setting
#             ylim = c(0, maxY),
#             xlab = "Insert Size (bp)",
#             ylab = "Frequency",
#             yaxs = "i"
#         )
#         abline(v = c(seq(-50, 50, 5), -1, -2), col = "grey")
#         abline(v = 0) 
#         lwd <- 1.5
#         assemblyPlotLines(microhomologyPlot, d$dt, lwd)
#         assemblyPlotGroupsLegend(microhomologyPlot, xlim, maxY, groupLabels_, lwd, d$groupCounts)
#         assemblyPlotTitle(microhomologyPlot, sourceId, d$titleSuffix)
#         stopSpinner(session)
#     }
# )

# #----------------------------------------------------------------------
# # endpoints distribution plot
# #----------------------------------------------------------------------
# endpointsType <- "endpoints"
# conditions_endpoints <- reactiveVal(NULL)
# output$conditions_endpoints <- renderAssemblyConditionsBucket(session, groupingCols, endpointsType, conditions_endpoints)
# observeEvent(input$conditions_endpointsOnChange, { conditions_endpoints(input$conditions_endpointsOnChange) })
# #----------------------------------------------------------------------
# groups_endpoints <- reactiveVal(NULL)
# output$groups_endpoints <- renderAssemblyGroupsBucket(session, groups, endpointsType, groups_endpoints)
# observeEvent(input$groups_endpointsOnChange, { groups_endpoints(input$groups_endpointsOnChange) })
# #----------------------------------------------------------------------
# endpointsData <- reactive({
#     req(isProcessingData())
#     svs <- matchingSvs()
#     groupingCols <- groupingCols()
#     conditions_ <- conditions_endpoints()
#     groups_ <- groups_endpoints()
#     targets <- assembly()$targets
#     binSize <- endpointsPlot$settings$get("Bins","Bin_Size")
#     req(svs, groupingCols, conditions_, groups_, targets, binSize)
#     assemblyCache$get(
#         'endpointsData', 
#         permanent = TRUE,
#         from = "ram",
#         # create = assemblyOptions$cacheCreateLevel, # 'asNeeded', 'once', 'always'
#         create = 'once', # 'asNeeded', 'once', 'always'
#         keyObject = list(
#             svs = svs,
#             groupingCols = groupingCols,
#             conditions_ = conditions_,
#             groups_ = groups_,
#             targets = targets,
#             binSize = binSize,
#             constants = APC$endpoints,
#             endpointsPlot$settings$all()
#         ), 
#         createFn = function(...) {
#             startSpinner(session, message = "counting endpoints")
#             targets[, chromI := {
#                 x <- sub("chr", "", chrom)
#                 x <- ifelse(x == "X", 99, x)
#                 x <- ifelse(x == "Y", 100, x)
#                 as.integer(x)
#             }]
#             targets <- targets[order(chromI, paddedStart)]
#             nTargets <- nrow(targets)
#             svs <- svs[groupLabels %in% groups_]
#             titleSuffix <- NULL
#             if(length(conditions_) < length(groupingCols)) {
#                 svs <- setAssemblyGroupLabels(svs, groupingCols, conditions_) # regroup a second time if some conditions were omitted
#                 groups_ <- unique(svs$groupLabels)
#                 titleSuffix <- getDroppedAssemblyGroupLabels(svs, groupingCols, conditions_)
#             }
#             svs <- svs[, .(endpointBin = as.integer(c(POS_1, POS_2) / binSize) * binSize), by = .(TARGET_REGION, groupLabels)]
#             mar <- APC$endpoints$mar
#             mar[3] <- mar[3] + length(groups_)
#             regions <- lapply(1:nTargets, function(i){
#                 svs <- svs[TARGET_REGION == targets[i, name]]
#                 mar <- mar
#                 if(i != nTargets) mar[1] <- APC$untitledAxisMar
#                 if(i != 1) mar[3] <- APC$noMar
#                 dt <- dcastSvsByGroup(
#                     svs[, .SD, .SDcols = c("endpointBin", "groupLabels")], 
#                     as.formula("endpointBin ~ groupLabels"), 
#                     groups_, 
#                     step = binSize
#                 )
#                 list(
#                     bins = dt, 
#                     mar = mar,
#                     regionCount = nrow(svs)
#                 )
#             })
#             Plot_Frame <- getAssemblyPlotFrame(
#                 endpointsPlot,
#                 APC$endpoints$insideWidth, 
#                 APC$endpoints$insideHeight * nTargets + APC$untitledAxisMar * (nTargets - 1) / APC$linesPerInch, 
#                 mar
#             )
#             x <- list(
#                 Plot_Frame = Plot_Frame,
#                 regions = regions,
#                 targets = targets,
#                 nTargets = nTargets,
#                 heights = sapply(1:nTargets, function(i) {
#                     APC$endpoints$insideHeight + sum(regions[[i]]$mar[c(1, 3)]) / APC$linesPerInch
#                 }) / Plot_Frame$Height_Inches,
#                 titleSuffix = titleSuffix,
#                 groupCounts = svs[, .N, by = .(groupLabels)]
#             )
#             stopSpinner(session)
#             x
#         }
#     )$value
# })
# #----------------------------------------------------------------------
# endpointsPlotFrame <- plotFrameReactive(endpointsData) 
# endpointsPlot <- staticPlotBoxServer(
#     "endpointsPlot",
#     settings = assemblyPlotSettings$endpoints, 
#     size = "m",
#     Plot_Frame = endpointsPlotFrame,
#     create = function() {
#         d <- endpointsData()
#         req(d)
#         startSpinner(session, message = "rendering endpoints plot")
#         layout(matrix(1:d$nTargets, ncol = 1), heights = d$heights)
#         for(i in 1:d$nTargets){
#             region <- d$regions[[i]]
#             geneStart <- d$targets[i, geneStart]
#             geneEnd <- d$targets[i, geneEnd]

#             groupLabels_ <- names(region$bins)[2:ncol(region$bins)]
#             nGroups <- length(groupLabels_)            
#             maxY <- region$bins[, max(.SD, na.rm = TRUE), .SDcols = groupLabels_] * 1.05
#             par(mar = region$mar, cex = 1)
#             xlim <- c(
#                 d$targets[i, paddedStart + 1],
#                 d$targets[i, paddedEnd]
#             ) / 1e6
#             ylim <- c(0, maxY)
#             endpointsPlot$initializeFrame(
#                 xlim = xlim, 
#                 ylim = ylim,
#                 ylab = "Frequency",
#                 yaxs = "i",
#                 xlab = if(i == d$nTargets) "SV Endpoint Coordinate (Mbp)" else ""
#             )
#             rect(geneStart / 1e6, ylim[1] * 0.98, geneEnd / 1e6, ylim[2], col = "grey95", border = NA)
#             abline(v = d$targets[i, c(regionStart, regionEnd) / 1e6], col = "grey40")
#             # abline(v = d$targets[i, c(paddedStart, paddedEnd) / 1e6], col = "grey40")
#             label <- paste(
#                 d$targets[i, paste(
#                     chrom, 
#                     paste(
#                         name, 
#                         if(is.na(geneStrand)) "" else paste0("(", geneStrand, ")")
#                     ),
#                     sep = ", "
#                 )],
#                 paste(region$regionCount, "SVs"), 
#                 sep = "\n"
#             )
#             text (xlim[1], maxY * 0.75, label, pos = 4, offset = 0)
#             lwd <- 1
#             assemblyPlotLines(endpointsPlot, region$bins, lwd, scale = 1e6)
#             if(i == 1) {
#                 assemblyPlotGroupsLegend(endpointsPlot, xlim, maxY, groupLabels_, lwd, d$groupCounts, "SVs")
#                 assemblyPlotTitle(endpointsPlot, sourceId, d$titleSuffix)
#             }
#         }
#         stopSpinner(session)
#     }
# )

# #----------------------------------------------------------------------
# # sizes distribution plot
# #----------------------------------------------------------------------
# svSizesType <- "svSizes"
# conditions_svSizes <- reactiveVal(NULL)
# output$conditions_svSizes <- renderAssemblyConditionsBucket(session, groupingCols, svSizesType, conditions_svSizes)
# observeEvent(input$conditions_svSizesOnChange, { conditions_svSizes(input$conditions_svSizesOnChange) })
# #----------------------------------------------------------------------
# groups_svSizes <- reactiveVal(NULL)
# output$groups_svSizes <- renderAssemblyGroupsBucket(session, groups, svSizesType, groups_svSizes)
# observeEvent(input$groups_svSizesOnChange, { groups_svSizes(input$groups_svSizesOnChange) })
# #----------------------------------------------------------------------
# svSizesData <- reactive({
#     req(isProcessingData())
#     svs <- matchingSvs()
#     groupingCols <- groupingCols()
#     conditions_ <- conditions_svSizes()
#     groups_ <- groups_svSizes()
#     binsPerLog <- svSizesPlot$settings$get("Bins","Bin_Per_Log")
#     req(svs, groupingCols, conditions_, groups_, binsPerLog)
#     assemblyCache$get(
#         'svSizesData', 
#         permanent = TRUE,
#         from = "ram",
#         create = assemblyOptions$cacheCreateLevel, # 'asNeeded', 'once', 'always'
#         keyObject = list(
#             svs = svs,
#             groupingCols = groupingCols,
#             conditions_ = conditions_,
#             groups_ = groups_,
#             binsPerLog = binsPerLog,
#             constants = APC$svSizes,
#             svSizesPlot$settings$all()
#         ), 
#         createFn = function(...) {
#             startSpinner(session, message = "counting sv sizes")
#             svs <- svs[groupLabels %in% groups_]
#             titleSuffix <- NULL
#             if(length(conditions_) < length(groupingCols)) {
#                 svs <- setAssemblyGroupLabels(svs, groupingCols, conditions_) # regroup a second time if some conditions were omitted
#                 groups_ <- unique(svs$groupLabels)
#                 titleSuffix <- getDroppedAssemblyGroupLabels(svs, groupingCols, conditions_)
#             }
#             svs <- svs[, .(sizeBin = as.integer(log10(SV_SIZE) * binsPerLog) / binsPerLog), by = .(groupLabels)]
#             dt <- dcastSvsByGroup(
#                 svs[, .SD, .SDcols = c("sizeBin", "groupLabels")], 
#                 as.formula("sizeBin ~ groupLabels"), 
#                 groups_, 
#                 step = 1 / binsPerLog
#             )
#             mar <- APC$svSizes$mar
#             mar[3] <- mar[3] + length(groups_)            
#             x <- list(
#                 Plot_Frame = getAssemblyPlotFrame(
#                     svSizesPlot, 
#                     APC$svSizes$insideWidth, 
#                     APC$svSizes$insideHeight, 
#                     mar
#                 ),
#                 mar = mar,
#                 dt = dt,
#                 titleSuffix = titleSuffix,
#                 groupCounts = svs[, .N, by = .(groupLabels)]
#             )
#             stopSpinner(session)
#             x
#         }
#     )$value
# })
# #----------------------------------------------------------------------
# svSizesPlotFrame <- plotFrameReactive(svSizesData) 
# svSizesPlot <- staticPlotBoxServer(
#     "svSizesPlot",
#     settings = assemblyPlotSettings$svSizes, 
#     size = "m",
#     Plot_Frame = svSizesPlotFrame,
#     create = function() {
#         d <- svSizesData()
#         req(d)
#         startSpinner(session, message = "rendering svSizes plot")
#         groupLabels_ <- names(d$dt)[2:ncol(d$dt)]
#         maxY <- d$dt[, max(.SD, na.rm = TRUE), .SDcols = groupLabels_] * 1.05
#         nGroups <- length(groupLabels_)
#         par(mar = d$mar)
#         xlim <- log10(c(
#             svSizesPlot$settings$get("Bins","Min_SV_Size"),
#             svSizesPlot$settings$get("Bins","Max_SV_Size")
#         ))
#         svSizesPlot$initializeFrame(
#             xlim = xlim,
#             ylim = c(0, maxY),
#             xlab = "Log10 SV Size (bp)",
#             ylab = "Frequency",
#             yaxs = "i",
#             title = ""
#         )
#         abline(v = 0:10, col = "grey")
#         lwd <- 1
#         assemblyPlotLines(svSizesPlot, d$dt, lwd)
#         assemblyPlotGroupsLegend(svSizesPlot, xlim, maxY, groupLabels_, lwd, d$groupCounts, "SVs")     
#         assemblyPlotTitle(svSizesPlot, sourceId, d$titleSuffix)
#         stopSpinner(session)
#     }
# )

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
