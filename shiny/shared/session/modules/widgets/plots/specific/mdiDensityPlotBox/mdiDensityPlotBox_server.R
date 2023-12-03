#----------------------------------------------------------------------
# server components for plotting one or more frequency distributions
#----------------------------------------------------------------------

#----------------------------------------------------------------------
mdiDensityPlotBoxServer <- function(
    id,
    #----------------------------------------------------------------------  
    data, # a data.table with columns x and groupingCols, or a reactive that returns it
    groupingCols, # column(s) that define the groups to summarize as distinct distributions, or a reactive that returns it; can be NULL
    xlab, # x axis label, or a reactive that returns it 
    #----------------------------------------------------------------------   
    defaultBinSize = 1, # starting bin resolution on the X axis
    eventTypePlural = "Events", # name of the thing being counted for the plot title
    ... # additional options passed to mdiXYPlot()
) { 
#----------------------------------------------------------------------
# collect anything with the potential to change the plotted data
pd <- reactive({
    list(
        dt              = if(is.data.table(data)) data else data(),
        groupingCols    = if(is.function(groupingCols)) groupingCols() else groupingCols,
        X_Bin_Size      = plot$settings$get("Density_Plot","X_Bin_Size"),
        Y_Axis_Value    = plot$settings$get("Density_Plot","Y_Axis_Value")
    )
})
#----------------------------------------------------------------------
# collect and group the plotted data
parseGroupingCols <- function(pd){
    groupingCols <- if(!is.null(pd$groupingCols) && length(pd$groupingCols) > 0) {
        droppedCols <- character()
        for(col in pd$groupingCols){
            if(length(unique(pd$dt[[col]])) == 1) droppedCols <- c(droppedCols, col)
        }
        groupingCols <- pd$groupingCols[!(pd$groupingCols %in% droppedCols)]
    } else character()
    if(length(groupingCols) == 0) return(list(
        groupingCols = NA,
        nGroupingCols = 0,
        groups = "singleGroup",
        nGroups = 1,
        hasGroups = FALSE,
        groupCounts = nrow(pd$dt),
        dt = pd$dt[, .(x = x, group = "singleGroup")]
    ))
    pd$dt[, group := .SD[, apply(.SD, 1, paste, collapse = ", "), .SDcols = groupingCols]]
    groups <- sort(unique(pd$dt$group))
    nGroupingCols <- length(groupingCols)
    groupCounts <- pd$dt[, .N, by = .(group)]
    setkey(groupCounts, group)
    list(
        groupingCols = groupingCols,
        nGroupingCols = length(groupingCols),
        groups = groups,
        nGroups = length(groups),
        hasGroups = nGroupingCols > 1,
        groupCounts = groupCounts[groups, N],
        dt = pd$dt[, .SD, .SDcols = c("x", "group")]
    )
}
fillAllGroups <- function(pd, grouping){ # ensure that all groups have a value, even if 0, for all X axis bins
    grouping$dt[, x := floor(x / pd$X_Bin_Size) * pd$X_Bin_Size] # thus, all bins are left-referenced
    dt <- dcast(
        grouping$dt,
        "x ~ group", 
        value.var = "group", 
        fun.aggregate = length, 
        fill = 0
    )
    if(pd$Y_Axis_Value == "Frequency") for(gl in grouping$groups) dt[[gl]] <- dt[[gl]] / sum(dt[[gl]], na.rm = TRUE)
    allBins <- seq(min(dt$x), max(dt$x), pd$X_Bin_Size)
    missingBins <- allBins[!(allBins %in% dt[[1]])]
    if(length(missingBins) > 0){
        missingBins <- data.table(x = missingBins)
        dt <- rbind(dt, missingBins, fill = TRUE)
        dt[is.na(dt)] <- 0
        setorderv(dt, "x", order = 1L)
    } 
    melt(
        dt, 
        id.vars = "x", 
        measure.vars = grouping$groups,
        variable.name = "group", 
        value.name = "y",
        variable.factor = FALSE
    )
}
plotData <- reactive({
    pd <- pd()
    req(pd$dt, nrow(pd$dt) > 0)
    grouping <- parseGroupingCols(pd)
    list(
        grouping = grouping,
        dt = fillAllGroups(pd, grouping),
        pd = pd
    )
})
#----------------------------------------------------------------------
plot <- staticPlotBoxServer(
    id,
    margins = TRUE,
    title = TRUE,
    lines = TRUE,
    settings = c(list(
        Density_Plot = list(
            Min_X_Value = list(
                type = "numericInput",
                value = -10,
                min = -50, 
                max = 0,
                step = 5
            ),
            Max_X_Value = list(
                type = "numericInput",
                value = 15,
                min = 0, 
                max = 50,
                step = 5
            ),
            X_Bin_Size = list(
                type = "numericInput",
                value = defaultBinSize
            ),
            Y_Axis_Value = list(
                type = "radioButtons",
                choices = c("Frequency","Count"),
                value = "Frequency"
            ),
            Plot_As = list(
                type = "radioButtons",
                choices = c("lines","area","histogram"),
                value = "lines"
            )
        )
    ), mdiXYPlotSettings), 
    size = "m",
    create = function() {
        d <- plotData()
        xlim <- c(
            plot$settings$get("Density_Plot","Min_X_Value"),
            plot$settings$get("Density_Plot","Max_X_Value")
        )
        ylim <- c(0, d$dt[, max(y) * 1.05])
        title <- plot$settings$get("Plot_Frame", "Title", NULL)
        totalN <- paste(trimws(commify(sum(d$grouping$groupCounts))), eventTypePlural)
        title <- if(is.null(title) || title == "") totalN else paste0(title, " (", totalN, ")")
        plot$initializeFrame(
            xlim = xlim,
            ylim = ylim,
            xlab = if(is.function(xlab)) xlab() else xlab,
            ylab = d$pd$Y_Axis_Value,
            xaxs = "i",
            yaxs = "i",
            title = title,
            cex.main = 0.95
        )
        mdiXYPlot(
            plot,
            d$dt,
            groupingCols = "group",
            # groupColors = groupColors,
            xlim = xlim,
            ylim = ylim,
            plotAs = plot$settings$get("Density_Plot","Plot_As"),
            histogramSpacing = d$pd$X_Bin_Size,
            ...
        )
        stopSpinner(session)
    }
)

# return the plot
plot 
#----------------------------------------------------------------------
}
#----------------------------------------------------------------------
