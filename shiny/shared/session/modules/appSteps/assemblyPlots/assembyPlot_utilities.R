#----------------------------------------------------------------------
# utility functions used to create assembly data objects and plots
#----------------------------------------------------------------------

# assembly type options and actions
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

# combine grouping columns names and values into a single display string
# used in legends, yielding "groupingCol1 = value1, groupingCol2 = value2" for each plotted group
# caller/user may decide to drop some columns, e.g., those with constant values, by setting includeCols or dropCols
setAssemblyGroupLabels <- function(x, groupingCols, includeCols = NULL, dropCols = NULL){ # x is a data.table
    workingCols <- if(is.null(includeCols)) groupingCols else groupingCols[groupingCols %in% includeCols]
    workingCols <- if(is.null(dropCols)) workingCols else workingCols[!(workingCols %in% dropCols)]
    x[, groupLabels := sapply(1:nrow(x), function(i) {
        paste(sapply(1:length(workingCols), function(j) {
            paste(workingCols[j], enDash(x[i][[workingCols[j]]]), sep = " = ")
        }), collapse = ", ")
    })]
}

# combine columns names and values into a single display string
# used in plot titles to show shared values, yielding similar format to setAssemblyGroupLabels
getDroppedAssemblyGroupLabels <- function(x, groupingCols, includeCols = NULL, dropCols = NULL){
    if(dropCols) dropCols <- groupingCols[!(groupingCols %in% includeCols)]
    if(length(dropCols) == 0) return("")
    paste(sapply(dropCols, function(col){
        values <- enDash(unique(x[[col]]))
        if(length(values) > 1) values <- paste(values, collapse = ",")
        paste(col, values, sep = " = ")
    }), collapse = ", ")
}

# assemble one set of drag and drop lists for interactive plot configuration
getAssemblyBucketList <- function(session, rankListId, labels){
    keepRankListId <- paste0(rankListId, "Keep")
    dropRankListId <- paste0(rankListId, "Drop")
    rankListOnChangeId <- session$ns(paste0(rankListId, "OnChange"))
    if(is.null(loadingPlotSet)){
        keepLabels <- labels
        dropLabels <-character()
    } else {
        x <- loadingPlotSet[[rankListId]]
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
    x <- getAssemblyBucketList(session, paste("groups", type, sep = "_"), groups$groupLabels)
    groupsReactive(x$order)
    x$ui
})

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



# additional data parsing
dcastSvsByGroup <- function(svs, formula, groups_, step = 1){
    dt <- dcast(
        svs,
        formula, 
        value.var = names(svs)[1], 
        fun.aggregate = length, 
        fill = 0
    )
    for(gl in groups_) dt[[gl]] <- dt[[gl]] / sum(dt[[gl]], na.rm = TRUE)
    allBins <- seq(min(dt[[1]]), max(dt[[1]]), step)
    missingBins <- allBins[!(allBins %in% dt[[1]])]
    if(length(missingBins) > 0){
        missingBins <- data.table(tmp = missingBins)
        setnames(missingBins, "tmp", names(dt)[1])
        dt <- rbind(dt, missingBins, fill = TRUE)
        dt[is.na(dt)] <- 0
        setorderv(dt, names(dt)[1], order = 1L)
    } 
    dt
}

plotFrameReactive <- function(data) reactive({ 
    tryCatch({
        data()$Plot_Frame
    }, error = function(e) list(
        Width_Inches  = 3, 
        Height_Inches = 3,
        Font_Size = 7
    ))
})

# add elements to plots
assemblyPlotLines <- function(plot, dt, lwd = 1, scale = 1){
    for(i in 2:ncol(dt)){ # overplot individual data points on a bar plot
        plot$addLines(
            x = dt[[1]] / scale,
            y = dt[[i]],
            col = CONSTANTS$plotlyColors[[i - 1]],
            lwd = lwd
        )
    }
}

# assembly plot legend builders
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
# a legend describing the plotted groups
assemblyPlotGroupsLegend <- function(plot, xlim, maxY, groupLabels_, lwd = 1, groupCounts = NULL, eventPlural = NULL){ # colored lines by group on top of the plot
    legend <- underscoresToSpaces(groupLabels_)
    if(!is.null(groupCounts)) {
        if(!is.null(eventPlural)) eventPlural <- paste0(" ", eventPlural)
        counts <- paste0("(", sapply(groupLabels_, function(x) groupCounts[groupLabels == x, N]), eventPlural, ")")
        legend <- paste(legend, counts)
    }
    plot$addMarginLegend(
        x = mean(xlim),
        xjust = 0.5,
        y = maxY,
        yjust = 0,
        legend = legend,
        col = unlist(CONSTANTS$plotlyColors[1:length(groupLabels_)]),
        lty = 1,
        lwd = lwd,
        bty = "n",
        cex = 0.9
    )
}

# automated assembly plot titles, with potential user override
assemblyPlotTitle <- function(plot, sourceId, suffix = NULL){
    title <- trimws(plot$settings$get("Plot","Title"))
    if(length(title) == 0 || title == "") title <- {

source <- getSourceFromId(sourceId)
        dstr(source)

        x <- sub(".svCapture.assemble", "", getSourceFilePackageName(sourceId()))   
        if(!is.null(suffix)) x <- paste(x, suffix, sep = ", ")
        x        
    }
    mtext(
        underscoresToSpaces(title), 
        side = 3, 
        line = as.integer(par("mar")[3]) - 1, 
        cex = NA
    )
}
