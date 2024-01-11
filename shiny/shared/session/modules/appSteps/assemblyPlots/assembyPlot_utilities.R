#----------------------------------------------------------------------
# utility functions used to create assembly data objects and plots
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# load and act on assembly type options and actions
#----------------------------------------------------------------------
getAssemblyTypeOptions <- function(options){
    read_yaml(file.path(
        app$sources$suiteSharedTypesDir, "assemblyTypes", options$assemblyType, "assembly.yml")
    )
}
doAssemblyAction <- function(action, options, ...){
    fn <- paste(options$assemblyType, action, sep = "_")
    req(exists(fn))
    startSpinner(session, message = action)
    x <- get(fn)(...)
    stopSpinner(session)
    x
}

#----------------------------------------------------------------------
# helpers for loading assembly pipeline objects with top-level page options
#----------------------------------------------------------------------
# enforce consistent formatting of informative assembly sample columns
standardizeAssemblyColumns <- function(assemblyOptions, assembly){
    for(col in names(assembly$samples)){
        if(col %in% assemblyOptions$internalUseSampleColumns) next
        values <- assembly$samples[[col]] # ensure that all empty/zero-dose cells have the value "-"
        assembly$samples[[col]][is.na(values) | is.null(values) | values == "" | values == "0"] <- "-"

        ######################## TODO: fix this in samples list and rerun assembly, then delete
        assembly$samples[[col]][values == "bulk"] <- "-"

        values <- assembly$samples[[col]] # remove columns that never vary over all samples
        if(length(unique(values)) == 1) assembly$samples[[col]] <- NULL
    }
    assembly
}
# collapse a set of different numerical doses (0.1, 0.2) to +/-
# expects input format either [-,dose,...], [0,dose,...] or [-,+]
assemblyDosesToLogical <- function(x, col = NULL){ # x is a vector or data.frame
    if(!is.null(col)) x <- x[[col]]
    x <- as.character(x)
    isZero <- x == "0"
    x[isZero] <- "-"
    isMinusSign <- x == "-"
    isNumeric <- all(!is.na(suppressWarnings(as.numeric(x[!isMinusSign]))))
    if(isNumeric) x[!isMinusSign] <- "+" # thus, all doses are converted to +, i.e., treated
    x # return a vector
}

# collapse a set of clone replicates by stripping the clone name and retaining only the target
# expects format TARGET:CLONE/REPLICATE, e.g. GeneX:siRNA-1/3
assemblyClonesToTargets <- function(x, col){ # x is a vector or data.frame 
    if(!is.null(col)) x <- x[[col]]
    sapply(x, function(value) strsplit(value, ":")[[1]][1]) 
}

#----------------------------------------------------------------------
# helpers for parsing and displaying assembly groups and conditions
#----------------------------------------------------------------------
# aggregate the individual sample data points that comprise a group
aggegrateGroupSampleValues <- function(groupedProjectSamples, groupingCols, valueColumn, nSigDigits = 4){
    if(is.function(groupedProjectSamples)) groupedProjectSamples <- groupedProjectSamples()
    if(is.function(groupingCols)) groupingCols <- groupingCols()
    groupedProjectSamples[, .(
        nProjects = length(unique(project)),
        nSamples = .N,
        meanSampleValue = round(mean(.SD[[valueColumn]]), nSigDigits),
        sdSampleValue = round(sd(.SD[[valueColumn]]), nSigDigits),
        sampleValues = list(.SD[[valueColumn]]), # plural, since can be more than one value per sample
        projects = list(project)
    ), by = groupingCols]
}
# combine grouping columns names and values into a single display string
# used in legends, yielding "groupingCol1 = value1, groupingCol2 = value2" for each plotted group
# caller/user may decide to drop some columns, e.g., those with constant values, by setting includeCols or dropCols
setAssemblyGroupLabels <- function(x, groupingCols, includeCols = NULL, dropCols = NULL){ # x is a data.table
    workingCols <- if(is.null(includeCols)) groupingCols else intersect(includeCols, groupingCols) # keep all usable columns in order provided in includeCols
    workingCols <- if(is.null(dropCols)) workingCols else workingCols[!(workingCols %in% dropCols)]
    groupLabels <- if(length(workingCols) == 0) "all" else apply(x[, 
        lapply(.SD, enDash), 
        .SDcols = workingCols
    ][, 
        lapply(workingCols, function(col) paste(col, .SD[[col]], sep = " = "))
    ], 1, paste, collapse = " | ")
    x[, 
        groupLabel := groupLabels
    ]
}
# combine columns names and values into a single display string
# used in plot titles to show shared values, yielding similar format to setAssemblyGroupLabels
getDroppedAssemblyGroupLabels <- function(x, groupingCols, includeCols = NULL, dropCols = NULL){
    if(is.null(dropCols)) dropCols <- groupingCols[!(groupingCols %in% includeCols)]
    if(length(dropCols) == 0) return("")
    paste(sapply(dropCols, function(col){
        values <- enDash(unique(x[[col]]))
        if(length(values) > 1) values <- paste(sort(values), collapse = "|")
        paste(col, values, sep = " = ")
    }), collapse = " | ")
}
# assemble one set of drag and drop lists for interactive plot configuration
getAssemblyBucketList <- function(session, rankListId, labels){
    keepRankListId <- paste0(rankListId, "Keep")
    dropRankListId <- paste0(rankListId, "Drop")
    rankListOnChangeId <- session$ns(paste0(rankListId, "OnChange"))
    if(is.null(loadingAssemblyPlotSet)){
        keepLabels <- labels
        dropLabels <-character()
    } else {
        x <- loadingAssemblyPlotSet[[rankListId]]
        keepLabels <- if(is.null(x)) labels else x[x %in% labels]
        dropLabels <- labels[!(labels %in% keepLabels)]
    }
    list(
        order = keepLabels,
        ui = bucket_list(
            "",
            add_rank_list(
                text = "Show/Use/Group on Plot",
                labels = keepLabels,
                input_id = keepRankListId,
                css_id = keepRankListId,
                options = sortable_options(
                    group = rankListId,
                    multiDrag = TRUE,
                    onSort = sortable_js_capture_input(input_id = rankListOnChangeId)
                ),
                class = "default-sortable" 
            ),
            add_rank_list(
                text = "Omit from / merge on Plot",
                labels = dropLabels,
                input_id = dropRankListId,
                css_id = dropRankListId,
                options = sortable_options(
                    group = rankListId,
                    multiDrag = TRUE
                ),
                class = "default-sortable" 
            )
        ) 
    )
}
renderAssemblyConditionsBucket <- function(session, groupingCols, type, conditionsReactive) renderUI({
    groupingCols <- groupingCols()
    req(groupingCols)
    x <- getAssemblyBucketList(session, paste("conditions", type, sep = "_"), groupingCols)
    conditionsReactive(x$order)
    x$ui
})
renderAssemblyGroupsBucket <- function(session, groups, type, groupsReactive) renderUI({
    groups <- groups()
    req(groups)
    x <- getAssemblyBucketList(session, paste("groups", type, sep = "_"), groups$groupLabel)
    groupsReactive(x$order)
    x$ui
})
# regroup a second time if some conditions were omitted by user via bucket seletions
regroupToUserConditions <- function(dt, groupingCols, conditions, groupLabels){
    if(is.function(groupingCols)) groupingCols <- groupingCols()
    if(is.function(conditions)) conditions <- conditions()
    if(is.function(groupLabels)) groupLabels <- groupLabels()
    titleSuffix <- NULL
    if(!identical(groupingCols, conditions)){
        dt <- setAssemblyGroupLabels(dt, groupingCols, includeCols = conditions) 
        groupLabels <- unique(dt$groupLabel)
        if(length(conditions) < length(groupingCols)) titleSuffix <- getDroppedAssemblyGroupLabels(dt, groupingCols, includeCols = conditions)
    }
    list(
        titleSuffix = titleSuffix,
        groupLabels = groupLabels,
        groupCounts = dt[, .N, by = .(groupLabel)],
        dt = dt
    )
}

#----------------------------------------------------------------------
# settings exposed for user customization of assembly plots
#----------------------------------------------------------------------
assemblyPlotFrameSettings <- list(
    Plot = list(
        Width_Inches = list( # allow size and title overrides of automated values
            type = "textInput",
            value = "auto"
        ),
        Height_Inches = list(
            type = "textInput",
            value = "auto"
        ),         
        Title = list(
            type = "textInput",
            value = ""
        ),
        Show_Condition_Names = list(
            type = "checkboxInput",
            value = TRUE
        )
    )
)

#----------------------------------------------------------------------
# handle dynamic plot frame sizing based on defaults, user inputs, and number of conditions/groups
#----------------------------------------------------------------------
# builders for plotting reactives
CONSTANTS$assemblyPlots <- list(
    linesPerInch = 8.571429,
    fontSize = 7,
    nullMar = 0.5,
    titleMar = 2.1,
    titleLegendMar = 2.1,
    stdAxisMar = 4.1,
    untitledAxisMar = 2.1,
    noMar = 0.1
)
getAssemblyPlotFrame <- function(plot, insideWidth, insideHeight, mar){
    maiHorizonatal <- sum(mar[c(2, 4)]) / CONSTANTS$assemblyPlots$linesPerInch
    maiVertical    <- sum(mar[c(1, 3)]) / CONSTANTS$assemblyPlots$linesPerInch
    userWidth  <- trimws(plot$settings$get("Plot","Width_Inches")) # enable  user overrides of automated plot dimensions
    userHeight <- trimws(plot$settings$get("Plot","Height_Inches"))
    width  <- if(userWidth  == "" || userWidth  == "auto") insideWidth + maiHorizonatal else as.numeric(userWidth)
    height <- if(userHeight == "" || userHeight == "auto") insideHeight + maiVertical   else as.numeric(userHeight)
    list(
        Width_Inches  = width, 
        Height_Inches = height,
        Font_Size     = CONSTANTS$assemblyPlots$fontSize
    )
}

#----------------------------------------------------------------------
# contruct a complete plot box, with group and condition buckets
#----------------------------------------------------------------------
assemblyPlotBoxServer <- function(
    id, session, input, output, 
    isProcessingData,
    groupingCols, groups,
    dataFn, plotFrameFn, plotFn
){
    condId <- paste("conditions", id, sep = "_")
    condChangeId <- paste0(condId, "OnChange")
    conditionsReactive <- reactiveVal()
    output[[condId]] <- renderAssemblyConditionsBucket(session, groupingCols, id, conditionsReactive)
    observeEvent(input[[condChangeId]], { conditionsReactive(input[[condChangeId]]) })
    #----------------------------------------------------------------------
    grpId <- paste("groups", id, sep = "_")
    grpChangeId <- paste0(grpId, "OnChange")
    groupsReactive <- reactiveVal()
    output[[grpId]] <- renderAssemblyGroupsBucket(session, groups, id, groupsReactive)
    observeEvent(input[[grpChangeId]], { groupsReactive(input[[grpChangeId]]) })
    #----------------------------------------------------------------------
    dataReactive <- reactive({ 
        req(isProcessingData())
        conditions <- conditionsReactive()
        groupLabels <- groupsReactive()
        req(groupLabels) # don't require conditions, user may choose to collapse everthing to a single group via buckets
        list(
            conditions = conditions,
            groupLabels = groupLabels, # the complete initial set of group labels in the bucket
            nConditions = length(conditions),
            nGroups = length(groupLabels),
            data = dataFn(conditions, groupLabels) # might carry modified groupLabels if regroupToUserConditions is used
        )
    })
    plotFrameReactive <- reactive({
        tryCatch({
            data <- dataReactive()
            req(data)
            plotFrameFn(data) 
        }, error = function(e) list(
            frame = list(
                Width_Inches  = 3, 
                Height_Inches = 3,
                Font_Size = 7
            ),
            mar = c(4.1, 4.1, 2.1, 0.5)
        ))
    })
    list(
        id = id,
        plot = plotFn(paste0(id, "Plot"), dataReactive, plotFrameReactive),
        conditionsReactive = conditionsReactive,
        groupsReactive = groupsReactive
    )   
}

#----------------------------------------------------------------------
# add elements to plots
#----------------------------------------------------------------------
# line traces
assemblyPlotLines <- function(plot, dt, lwd = 1, scale = 1){
    for(i in 2:ncol(dt)){
        plot$addLines(
            x = dt[[1]] / scale,
            y = dt[[i]],
            col = CONSTANTS$plotlyColors[[i - 1]],
            lwd = lwd
        )
    }
}
# automated assembly plot titles, with potential user override
prettifyGroupConditions <- function(x, showConditionNames = TRUE){ # optionally strip conditions names from labels and right pad for alignment
    n <- length(x)
    if(n == 0) return(x)
    x <- sapply(strsplit(x, " \\| "), function(groupConditions){
        if(showConditionNames) groupConditions
        else sapply(groupConditions, function(groupCondition) strsplit(groupCondition, " = ")[[1]][2])
    }) %>%
    matrix(ncol = n) %>%
    apply(1, rightPadStrings) %>%
    matrix(nrow = n) %>%
    apply(1, paste, collapse = " | ")
} 
getAssemblyPackageName <- function(sourceId) strsplit(getSourceFilePackageName(sourceId), "\\.")[[1]][1]
getAssemblyPlotTitle <- function(plot, sourceId, suffix = NULL, showConditionNames = TRUE){
    title <- trimws(plot$settings$get("Plot","Title"))
    if(length(title) == 0 || title == "") title <- {
        x <- getAssemblyPackageName(sourceId())
        if(!is.null(suffix)) x <- paste(x, prettifyGroupConditions(suffix, showConditionNames), sep = ", ")
        gsub(" \\| ", ", ", x)
    }
    underscoresToSpaces(title)
}
assemblyPlotTitle <- function(plot, sourceId, suffix = NULL, showConditionNames = TRUE){
    mtext(
        getAssemblyPlotTitle(plot, sourceId, suffix, showConditionNames), 
        side = 3, 
        line = as.integer(par("mar")[3]) - 1, 
        cex = NA
    )
}
# a legend describing the plotted groups below the title and above the plot
getAssemblyPlotGroupsLegend <- function(groupLabels, groupCounts, eventPlural, showConditionNames = TRUE){
    legend <- gsub(" \\| ", "  ", prettifyGroupConditions(groupLabels, showConditionNames)) %>% underscoresToSpaces
    if(!is.null(groupCounts)) {
        if(!is.null(eventPlural)) eventPlural <- paste0(" ", eventPlural)
        if(length(groupLabels) == 1) return(paste(sum(groupCounts$N), eventPlural))
        counts <- paste0("(", sapply(groupLabels, function(x) groupCounts[groupLabel == x, N]), eventPlural, ")")
        legend <- paste(legend, counts)
    }    
    legend
}
assemblyPlotGroupsLegend <- function(plot, xlim, maxY, groupLabels_, lwd = 1,  # colored lines by group on top of the plot
                                     groupCounts = NULL, eventPlural = NULL, showConditionNames = TRUE){
    plot$addMarginLegend(
        x = mean(xlim),
        xjust = 0.5,
        y = maxY,
        yjust = 0,
        legend = getAssemblyPlotGroupsLegend(groupLabels_, groupCounts, eventPlural, showConditionNames),
        col = unlist(CONSTANTS$plotlyColors[1:length(groupLabels_)]),
        lty = 1,
        lwd = lwd,
        bty = "n",
        cex = 0.9
    )
}
# a table of conditions underneath the X axis of a bar chart, etc.
assemblyPlotConditionsGrid <- function(groupingCols, groups, conditionsI){
    nConditions <- length(conditionsI)
    nGroups <- nrow(groups)
    mtext( # labels for the condition grid rows
        gsub("_", " ", groupingCols[conditionsI]), 
        side = 1, 
        line = 1:nConditions, 
        at = 0,
        adj = 1,
        cex = 0.9
    )
    for(i in 1:nConditions){ # fill the condition grid with values by row
        j <- conditionsI[i]
        mtext(
            enDash(unlist(groups[, .SD, .SDcols = groupingCols[j]])), 
            side = 1, 
            line = i, 
            at = 1:nGroups
        )
    }
}

#----------------------------------------------------------------------
# vertical barplot overplotted with error bars and individual data points
#----------------------------------------------------------------------
assemblyBarplotServer <- function(
    id, session, input, output, 
    isProcessingData, assemblyOptions,
    sourceId, assembly, groupedProjectSamples, groupingCols, groups,
    ylab, 
    groupWidthInches = 0.3, insideHeight = 1,
    nSD = 2, barHalfWidth = 0.35, jitterHalfWidth = 0.25
){
    settings <- c(assemblyPlotFrameSettings, list(
        Groups = list(
            Group_Width_Inches = list(
                type = "numericInput",
                value = groupWidthInches,
                min = 0.05,
                max = 1,
                step = 0.05
            )
        )
    ))
    mar <- c(
        CONSTANTS$assemblyPlots$titleLegendMar, 
        8.1, 
        CONSTANTS$assemblyPlots$titleMar, 
        CONSTANTS$assemblyPlots$nullMar
    )
    assemblyPlot <- assemblyPlotBoxServer( 
        id, session, input, output, 
        isProcessingData,
        groupingCols, groups,
        dataFn = function(conditions, groupLabels) {
            list(
                groups = groups(),
                groupingCols = groupingCols()
            )
        },  
        plotFrameFn = function(data) {
            mar <- mar 
            mar[1] <- mar[1] + data$nConditions
            list(
                frame = getAssemblyPlotFrame(
                    plot = assemblyPlot$plot, 
                    insideWidth = assemblyPlot$plot$settings$get("Groups","Group_Width_Inches") * data$nGroups, 
                    insideHeight = insideHeight, 
                    mar = mar
                ),
                mar = mar
            )
        },
        plotFn = function(plotId, dataReactive, plotFrameReactive) staticPlotBoxServer(
            plotId,
            settings = settings, 
            size = "m",
            Plot_Frame = reactive({ plotFrameReactive()$frame }),
            create = function() {
                d <- dataReactive()
                req(d)
                startSpinner(session, message = paste("rendering", id))
                conditionsI <- sapply(d$conditions,  function(x) which(d$data$groupingCols == x))
                groupsI     <- sapply(d$groupLabels, function(x) which(d$data$groups$groupLabel == x))
                groups <- d$data$groups[groupsI]
                nGroups <- nrow(groups)
                uniqueProjects <- unique(unlist(groups$projects))
                colors <- CONSTANTS$plotlyColors[1:length(uniqueProjects)]
                names(colors) = uniqueProjects
                maxY <- max(
                    unlist(groups$sampleValues), 
                    groups[, meanSampleValue + sdSampleValue * nSD],
                    na.rm = TRUE
                ) * 1.05
                par(mar = plotFrameReactive()$mar)
                assemblyPlot$plot$initializeFrame(
                    xlim = c(0.5, nGroups + 0.5), # bars have a unit width of 1 on the x-axis
                    ylim = c(0, maxY),
                    xlab = "",
                    ylab = ylab,
                    yaxs = "i",
                    xaxt = "n"
                )
                rect( # make the bar plot
                    1:nGroups - barHalfWidth, 
                    0, 
                    1:nGroups + barHalfWidth, 
                    groups[, meanSampleValue], 
                    lty = 1, 
                    lwd = 1,
                    col = "grey80" # TODO: expose argument for bar coloring
                )
                for(i in 1:nGroups){ # overplot individual data points on the bar plot
                    lines(rep(i, 2), groups[i, meanSampleValue + sdSampleValue * c(-nSD, nSD)])
                    sampleValues <- unlist(groups[i, sampleValues])
                    projects <- unlist(groups[i, projects])
                    assemblyPlot$plot$addPoints(
                        x = jitter2(sampleValues, i - jitterHalfWidth, i + jitterHalfWidth),
                        y = sampleValues,
                        col = sapply(projects, function(x) colors[[x]]) # TODO: expose argument to allow different coloring modes
                    )
                }
                assemblyPlotConditionsGrid(d$data$groupingCols, groups, conditionsI)
                assemblyPlotTitle(assemblyPlot$plot, sourceId)
                stopSpinner(session)
            }
        )
    )
}

#----------------------------------------------------------------------
# grouped density plot
#----------------------------------------------------------------------
assemblyDensityPlotServer <- function(
    id, session, input, output, 
    isProcessingData, assemblyOptions,
    sourceId, assembly, groupedProjectSamples, groupingCols, groups,
    dataFn, xlab, eventPlural,
    insideWidth = 1.5, insideHeightPerBlock = 1,
    trackCols = NULL, trackSameXLim = TRUE, trackSameYLim = TRUE,
    extraSettings = list(), # a list of additional settings families
    defaultSettingValues = list(), # values overrides for assemblyPlotFrameSettings, mdiDensityPlotSettings
    aggFn = length,
    aggCol = "x",
    ... # additional arguments passed to mdiDensityPlotBoxServer, 
        # especially defaultBinSize, v, x0Line
){
    mar <- c(
        CONSTANTS$assemblyPlots$stdAxisMar,
        CONSTANTS$assemblyPlots$stdAxisMar, 
        CONSTANTS$assemblyPlots$titleLegendMar, 
        CONSTANTS$assemblyPlots$nullMar
    )
    assemblyPlot <- assemblyPlotBoxServer( 
        id, session, input, output, 
        isProcessingData,
        groupingCols, groups,
        dataFn = dataFn, 
        plotFrameFn = function(data) {
            mar <- mar 
            mar[3] <- mar[3] + length(data$data$groupLabels)
            trackLabels <- data$data$trackLabels
            nTrackLabels <- max(1, if(is.null(trackLabels)) 1 else length(trackLabels))
            apc <- CONSTANTS$assemblyPlots
            list(
                frame = getAssemblyPlotFrame(
                    plot = assemblyPlot$plot, 
                    insideWidth = insideWidth, 
                    insideHeight = insideHeightPerBlock * nTrackLabels + 
                                   (apc$untitledAxisMar + apc$noMar) * (nTrackLabels - 1) / apc$linesPerInch,
                    mar = mar
                ), 
                insideHeightPerTrack = insideHeightPerBlock,
                mar = mar
            )
        },
        plotFn = function(plotId, dataReactive, plotFrameReactive) mdiDensityPlotBoxServer(
            id = plotId,
            defaultSettings = {
                ds <- c(assemblyPlotFrameSettings, mdiDensityPlotSettings, extraSettings) 
                for(family in names(defaultSettingValues)) for(option in names(defaultSettingValues[[family]])){
                    ds[[family]][[option]]$value = defaultSettingValues[[family]][[option]]
                }
                ds
            },
            plotFrameReactive = plotFrameReactive,
            data = reactive({ dataReactive()$data$dt }),
            groupingCols = "groupLabel",
            plotTitle = reactive({ 
                d <- dataReactive()$data
                getAssemblyPlotTitle(
                    assemblyPlot$plot, 
                    sourceId, 
                    d$titleSuffix,
                    assemblyPlot$plot$settings$get("Plot","Show_Condition_Names")
                )
            }),
            legend_ = function(groupLabels){
                d <- dataReactive()$data
                getAssemblyPlotGroupsLegend(
                    groupLabels, 
                    d$groupCounts, 
                    eventPlural, 
                    assemblyPlot$plot$settings$get("Plot","Show_Condition_Names")
                )
            },
            legendSide = 3,
            xlab = xlab,
            eventPlural = eventPlural,
            trackCols = trackCols,
            trackLabels = reactive({ dataReactive()$data$trackLabels }),
            trackSameXLim = trackSameXLim,
            trackSameYLim = trackSameYLim,
            aggFn  = aggFn,
            aggCol = aggCol,
            ...
        )
    )
}
