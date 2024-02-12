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
aggegrateGroupSampleValues <- function(groupedProjectSamples, groupingCols, valueColumn, input, nSigDigits = 4){
    if(is.function(groupedProjectSamples)) groupedProjectSamples <- groupedProjectSamples()
    if(is.function(groupingCols)) groupingCols <- groupingCols()
    if(length(groupingCols) == 0) groupingCols <- NULL # support a single group over all data
    x <- groupedProjectSamples[, .( # aggregate sample values over all filtered data_types
        nProjects = length(unique(project)),
        nSamples = .N,
        meanSampleValue = round(mean(.SD[[valueColumn]]), nSigDigits),
        sdSampleValue = round(sd(.SD[[valueColumn]]), nSigDigits),
        sampleValues = list(.SD[[valueColumn]]), # plural, since can be more than one value per sample
        projects = list(project)
    ), by = groupingCols]
    if(!is.null(input$Data_Types) && length(input$Data_Types) > 0) for(dataType in input$Data_Types){
        dtvc <- paste(valueColumn, dataType, sep = "__") # aggregate each data type individual for splittin in barplots, etc.
        if(is.null(groupedProjectSamples[[dtvc]])) next
        x[[paste("meanSampleValue", dataType, sep = "__")]] <- groupedProjectSamples[, .(VAL_ = round(mean(.SD[[dtvc]]), nSigDigits)), by = groupingCols]$VAL_
        x[[paste("sdSampleValue",   dataType, sep = "__")]] <- groupedProjectSamples[, .(VAL_ = round(sd(.SD[[dtvc]]),   nSigDigits)), by = groupingCols]$VAL_
    }
    x
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
getAssemblyPlotFrame <- function(plot, insideWidth, insideHeight, mar, fontSize = CONSTANTS$assemblyPlots$fontSize){
    linesPerInch <- CONSTANTS$assemblyPlots$linesPerInch * CONSTANTS$assemblyPlots$fontSize / fontSize
    maiHorizonatal <- sum(mar[c(2, 4)]) / linesPerInch
    maiVertical    <- sum(mar[c(1, 3)]) / linesPerInch
    userWidth  <- trimws(plot$settings$get("Plot","Width_Inches")) # enable  user overrides of automated plot dimensions
    userHeight <- trimws(plot$settings$get("Plot","Height_Inches"))
    width  <- if(userWidth  == "" || userWidth  == "auto") insideWidth + maiHorizonatal else as.numeric(userWidth)
    height <- if(userHeight == "" || userHeight == "auto") insideHeight + maiVertical   else as.numeric(userHeight)
    list(
        Width_Inches  = width, 
        Height_Inches = height,
        Font_Size     = fontSize
    )
}

#----------------------------------------------------------------------
# contruct a complete plot pr table box, with group and condition buckets
#----------------------------------------------------------------------
assemblyPlotBoxServer <- function(
    id, session, input, output, 
    isProcessingData,
    groupingCols, groups,
    dataFn, plotFrameFn, plotFn
){
    condId <- paste("conditions", id, sep = "_") # can't wrap this in a function (not sure why)
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
assemblyTableBoxServer <- function(
    id, session, input, output, 
    isProcessingData,
    groupingCols, groups,
    dataFn, tableFn
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
    list(
        id = id,
        table = tableFn(paste0(id, "Table"), dataReactive),
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
        gsub("^\\.\\.", "", gsub(" \\| ", ", ", x))
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
getAssemblyPlotGroupsLegend <- function(groupLabels, groupCounts = NULL, eventPlural = NULL, showConditionNames = TRUE){
    legend <- gsub("^\\.\\.", "", gsub(" \\| ", "  ", prettifyGroupConditions(groupLabels, showConditionNames))) %>% underscoresToSpaces
    if(!is.null(groupCounts)) {
        if(!is.null(eventPlural)) eventPlural <- paste0(" ", eventPlural)
        if(length(groupLabels) == 1) return(paste(sum(groupCounts$N), eventPlural))
        counts <- paste0("(", sapply(groupLabels, function(x) groupCounts[groupLabel == x, N]), eventPlural, ")")
        legend <- paste(legend, counts)
    }    
    legend
}
assemblyPlotGroupsLegend <- function(plot, xlim, maxY, groupLabels_, lwd = 1,  # colored lines by group on top of the plot
                                     groupCounts = NULL, eventPlural = NULL, showConditionNames = TRUE,
                                     groupColors = NULL, lty = 1, pch = NULL, pt.cex = NULL, cex = 0.9, text.font = 1){
    plot$addMarginLegend(
        x = mean(xlim),
        xjust = 0.5,
        y = maxY,
        yjust = 0,
        legend = getAssemblyPlotGroupsLegend(groupLabels_, groupCounts, eventPlural, showConditionNames),
        col = if(is.null(groupColors)) unlist(CONSTANTS$plotlyColors[1:length(groupLabels_)]) else groupColors,
        lty = lty,
        lwd = lwd,
        pch = pch,
        pt.cex = pt.cex,
        bty = "n",
        cex = cex,
        text.font = text.font
    )
}
# a table of conditions underneath the X axis of a bar chart, etc.
assemblyPlotConditionsGrid <- function(groupingCols, groups, conditionsI){
    if(is.null(groupingCols)|| length(groupingCols) == 0) return(NULL)
    nConditions <- length(conditionsI)
    nGroups <- nrow(groups)
    mtext( # labels for the condition grid rows
        gsub("^\\.\\.", "", gsub("_", " ", groupingCols[conditionsI])), 
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
    nSD = 2, barHalfWidth = 0.35, jitterHalfWidth = 0.25,
    extraSettings = list(), # settings families, as a list
    splitDataTypes = FALSE,
    fontSize = CONSTANTS$assemblyPlots$fontSize
){
    settings <- c(assemblyPlotFrameSettings, list(
        Groups = list(
            Group_Width_Inches = list(
                type = "numericInput",
                value = groupWidthInches,
                min = 0.05,
                max = 1,
                step = 0.05
            ),
            Right_Margin_Inches = list(
                type = "numericInput",
                value = 0.5
            ),
            Plot_Sample_Points = list(
                type = "checkboxInput",
                value = TRUE
            ),
            Split_By_Data_Type = list(
                type = "checkboxInput",
                value = FALSE
            )
        )
    ), extraSettings)
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
                groupingCols = groupingCols(),
                dataTypes = input$Data_Types
            )
        },  
        plotFrameFn = function(data) {
            mar <- mar 
            mar[1] <- mar[1] + data$nConditions
            splitDataTypes <- assemblyPlot$plot$settings$get("Groups","Split_By_Data_Type")
            if(splitDataTypes) mar[4] <- assemblyPlot$plot$settings$get("Groups","Right_Margin_Inches") * CONSTANTS$assemblyPlots$linesPerInch
            list(
                frame = getAssemblyPlotFrame(
                    plot = assemblyPlot$plot, 
                    insideWidth = assemblyPlot$plot$settings$get("Groups","Group_Width_Inches") * data$nGroups, 
                    insideHeight = insideHeight, 
                    mar = mar,
                    fontSize = fontSize
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
                plotPoints <- assemblyPlot$plot$settings$get("Groups","Plot_Sample_Points")
                splitDataTypes <- assemblyPlot$plot$settings$get("Groups","Split_By_Data_Type")
                if(splitDataTypes) plotPoints <- FALSE
                maxY <- (
                    if(splitDataTypes) max(sapply(d$data$dataTypes, function(dataType){
                        groups[[paste("meanSampleValue", dataType, sep = "__")]] + groups[[paste("sdSampleValue", dataType, sep = "__")]] * nSD
                    }), na.rm = TRUE) else max(
                        unlist(groups$sampleValues), 
                        groups[, meanSampleValue + sdSampleValue * nSD],
                        na.rm = TRUE
                    )
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
                if(splitDataTypes) {
                    groupStarts <- 1:nGroups - barHalfWidth
                    barWidth <- 2 * barHalfWidth / length(d$data$dataTypes)
                    for(j in seq_along(d$data$dataTypes)){
                        rect( # make the bar plot
                            groupStarts + (j - 1) * barWidth,
                            0, 
                            groupStarts + j * barWidth, 
                            groups[[paste("meanSampleValue", d$data$dataTypes[j], sep = "__")]], 
                            lty = 1, 
                            lwd = 1,
                            col = CONSTANTS$plotlyColors[[j]]
                        )
                    }
                } else rect( # make the bar plot
                    1:nGroups - barHalfWidth, 
                    0, 
                    1:nGroups + barHalfWidth, 
                    groups[, meanSampleValue], 
                    lty = 1, 
                    lwd = 1,
                    col = "grey80" # TODO: expose argument for bar coloring
                )
                for(i in 1:nGroups){ # overplot individual data points on the bar plot
                    if(splitDataTypes) {
                        groupStart <- i - barHalfWidth
                        barWidth <- 2 * barHalfWidth / length(d$data$dataTypes)
                        for(j in seq_along(d$data$dataTypes)){
                            m  <- groups[i][[paste("meanSampleValue", d$data$dataTypes[j], sep = "__")]]
                            sd <- groups[i][[paste("sdSampleValue",   d$data$dataTypes[j], sep = "__")]]
                            lines(
                                rep(groupStart + (j - 1) * barWidth + barWidth / 2, 2), 
                                m + sd * c(-nSD, nSD)
                            )
                        }
                    } else {
                        lines(rep(i, 2), groups[i, meanSampleValue + sdSampleValue * c(-nSD, nSD)])
                    }
                    if (!plotPoints) next
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
                if(splitDataTypes) assemblyPlot$plot$addMarginLegend(
                    x = nGroups + 0.6, 
                    y = maxY, 
                    legend = trimws(input$Data_Types),
                    fill = unlist(CONSTANTS$plotlyColors[1:length(d$data$dataTypes)]),
                    bty = "n",
                    x.intersp = 0
                )
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
    fontSize = CONSTANTS$assemblyPlots$fontSize,
    groupV = NULL, groupH = NULL, # or optional function(data) to return group-specific demarcation lines
    adjustGroupsFn = function(x) x,
    ylab = NULL,
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
                    mar = mar,
                    fontSize = fontSize
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
            groupWeights = reactive({ d <- dataReactive()$data$groupWeights }),
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
                ) %>% adjustGroupsFn()
            },
            legendSide = 3,
            xlab = xlab,
            ylab = ylab,
            eventPlural = eventPlural,
            trackCols = trackCols,
            trackLabels = reactive({ dataReactive()$data$trackLabels }),
            trackSameXLim = trackSameXLim,
            trackSameYLim = trackSameYLim,
            aggFn  = aggFn,
            aggCol = aggCol,
            groupV = if(is.function(groupV)) reactive({ groupV(dataReactive()$data) }) else NULL,
            groupH = if(is.function(groupH)) reactive({ groupH(dataReactive()$data) }) else NULL,
            ...
        )
    )
}

#----------------------------------------------------------------------
# general XY plot
#----------------------------------------------------------------------
assemblyXYPlotServer <- function(
    id, session, input, output, 
    isProcessingData, assemblyOptions,
    sourceId, assembly, groupedProjectSamples, groupingCols, groups,
    dataFn, plotFn,
    dims = list(width = 1.5, height = 1.5), # or a reactive that returns it
    mar = c(4.1, 4.1, 0.1, 0.1), # or a reactive that returns it
    extraSettings = NULL,
    fontSize = CONSTANTS$assemblyPlots$fontSize,
    ... # additional arguments passed to staticPlotBoxServer
){
    getDim <- function(key, option, data){
        dim <- trimws(assemblyPlot$plot$settings$get("Plot",option))
        if(!isTruthy(dim) || dim == "" || dim == "auto") {
            if(is.reactive(dims)) dims()[[key]] 
            else if(is.function(dims)) dims(data)[[key]] 
            else dims[[key]]
        }
        else dim
    }
    assemblyPlot <- assemblyPlotBoxServer( 
        id, session, input, output, 
        isProcessingData,
        groupingCols, groups,
        dataFn = function(conditions, groupLabels) dataFn(assemblyPlot$plot, conditions, groupLabels),
        plotFrameFn = function(data) {
            mar_ <- if(is.reactive(mar)) mar() 
                    else if(is.function(mar)) mar(data) 
                    else mar
            list(
                frame = getAssemblyPlotFrame(
                    plot = assemblyPlot$plot, 
                    insideWidth  = getDim("width",  "Width_Inches", data), 
                    insideHeight = getDim("height", "Height_Inches", data), 
                    mar = mar_,
                    fontSize = fontSize
                ),
                mar = mar_
            )
        },
        plotFn = function(plotId, dataReactive, plotFrameReactive) staticPlotBoxServer(
            plotId,
            settings = c(assemblyPlotFrameSettings, extraSettings), 
            size = "m",
            Plot_Frame = reactive({ plotFrameReactive()$frame }),
            create = function() {
                d <- dataReactive()
                par(mar = plotFrameReactive()$mar)
                plotFn(assemblyPlot$plot, d)
                stopSpinner(session)
            },
            ...
        )
    )
    assemblyPlot
}

#----------------------------------------------------------------------
# general bufferedTable
#----------------------------------------------------------------------
assemblyBufferedTableServer <- function(
    id, session, input, output, 
    isProcessingData, assemblyOptions,
    sourceId, assembly, groupedProjectSamples, groupingCols, groups,
    settings = NULL,
    dataFn,
    ...
){
    if(is.list(settings)) settings <- settingsServer(
        session$ns('settings'), 
        id, 
        templates = list(settings), 
        size = "m",
        title = "Table Settings",
        resettable = TRUE
    )
    assemblyTable <- assemblyTableBoxServer( 
        id, session, input, output, 
        isProcessingData,
        groupingCols, groups,
        dataFn = function(conditions, groupLabels) dataFn(conditions, groupLabels, settings),
        tableFn = function(tableId, dataReactive) bufferedTableServer(
            id = tableId,
            parentId = id,
            parentInput = input,
            tableData = reactive({ dataReactive()$data }),
            settings = settings,
            ...
        )
    )
    assemblyTable
}
