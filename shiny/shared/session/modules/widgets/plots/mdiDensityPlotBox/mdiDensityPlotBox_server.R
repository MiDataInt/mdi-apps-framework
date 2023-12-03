#----------------------------------------------------------------------
# server components for plotting one or more frequency distributions
#----------------------------------------------------------------------

#----------------------------------------------------------------------
mdiDensityPlotBoxServer <- function(
    id,
    data, # a data.table or data.frame with all named columns, or a reactive that returns it
    lhsCols, # columns on the left hand side of the dcast formula, usually the X-axis value, or a reactive that returns it
    groupingCols, # columns used to define the groups to summarize, or a reactive that returns it
    step, # the bin resolution on the X axis, or a reactive that returns it
    xlab = paste(lhsCols, sep = ", "), # x axis label, or a reactive that returns it
    extraCols = NULL # additional column to maintain from the original data
) { 
#----------------------------------------------------------------------
x <- reactive({
    list(
        dt              = as.data.table(if(is.function(data)) data() else data),
        lhsCols         = if(is.function(lhsCols)) lhsCols() else lhsCols,
        groupingCols    = if(is.function(groupingCols)) groupingCols() else groupingCols,
        step            = if(is.function(step)) step() else step,
        xlab            = if(is.function(xlab)) xlab() else xlab,
        extraCols       = if(is.function(extraCols)) extraCols() else extraCols
    )
})
#----------------------------------------------------------------------
parseGroupingCols <- function(x){
    groupingCols <- if(!is.null(x$groupingCols) && length(x$groupingCols) > 0) {
        droppedCols <- character()
        for(col in x$groupingCols){
            if(length(unique(x$dt[[col]])) == 1) droppedCols <- c(droppedCols, col)
        }
        groupingCols <- x$groupingCols[!(x$groupingCols %in% droppedCols)]
    } else character()
    if(length(groupingCols) == 0) groupingCols <- "X"
    x$dt[, group := .SD[, apply(.SD, 1, paste, collapse = ", "), .SDcols = groupingCols]]
    groups <- sort(unique(x$dt$group))
    nGroupingCols <- length(groupingCols)
    groupCounts <- x$dt[, .N, by = .(group)]
    setkey(groupCounts, group)
    list(
        groupingCols = groupingCols,
        nGroupingCols = length(groupingCols),
        groups = groups,
        nGroups = length(groups),
        hasGroups = nGroupingCols > 1 || groupingCols != "X",
        groupCounts = groupCounts[groups, N],
        dt = x$dt[, .SD, .SDcols = c(x$lhsCols, "group", x$extraCols)]
    )
}
dcastByGroup <- function(x, dt, groups_){
    dt <- dcast(
        dt,
        as.formula(paste(x$lhsCols, "~ group")), 
        value.var = names(dt)[1], 
        fun.aggregate = length, 
        fill = 0
    )
    # for(gl in groups_) dt[[gl]] <- dt[[gl]] # / sum(dt[[gl]], na.rm = TRUE)
    allBins <- seq(min(dt[[1]]), max(dt[[1]]), step)
    missingBins <- allBins[!(allBins %in% dt[[1]])]
    if(length(missingBins) > 0){
        missingBins <- data.table(tmp = missingBins)
        setnames(missingBins, "tmp", names(dt)[1])
        dt <- rbind(dt, missingBins, fill = TRUE)
        dt[is.na(dt)] <- 0
        setorderv(dt, names(dt)[1], order = 1L)
    } 
    dt[, .SD, .SDcols = c(x$lhsCols, groups_)]
}
#----------------------------------------------------------------------
plotData <- reactive({
    x <- x()
    req(x$dt, nrow(x$dt) > 0)
    grouping <- parseGroupingCols(x)

    dstr(grouping)

    list(
        grouping = grouping,
        dists = dcastByGroup(x, grouping$dt, grouping$groups),
        dt = x$dtdt,
        x = x
    )
})
#----------------------------------------------------------------------
plot <- staticPlotBoxServer(
    id,
    margins = TRUE,
    title = TRUE,
    settings = list(
        Limits = list(
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
            )
        )
    ), 
    size = "m",
    create = function() {
        d <- plotData()
        xlim <- c(
            plot$settings$get("Limits","Min_X_Value"),
            plot$settings$get("Limits","Max_X_Value")
        )
        maxY <- d$dists[, max(.SD, na.rm = TRUE), .SDcols = d$grouping$groups] * 1.05 
        title <- plot$settings$get("Plot_Frame", "Title", NULL)

        # totalJxns <- paste(trimws(commify(sum(d$grouping$groupCounts))), "Total Junctions")

        # title <- if(is.null(title)) totalJxns else paste0(title, " (", totalJxns, ")")
        plot$initializeFrame(
            xlim = xlim,
            ylim = c(0, maxY),
            xlab = d$x$xlab,
            ylab = "Frequency",
            xaxs = "i",
            yaxs = "i",
            title = title,
            cex.main = 0.95
        )
        # abline(v = c(seq(-50, 50, 5), -1, -2), col = "grey")
        # abline(v = 0) 
        lwd <- 1.5
        for(i in 2:ncol(d$dists)){ # overplot individual data points on the bar plot
            plot$addLines(
                x = d$dists[[1]],
                y = d$dists[[i]],
                col = CONSTANTS$plotlyColors[[i - 1]],
                lwd = lwd
            )
        }
        if(d$grouping$hasGroups) plot$addMarginLegend(
            xlim[2] * 1.1, 
            maxY, 
            lty = 1, 
            lwd = 1.5, 
            legend = paste0(d$grouping$groups, " (", trimws(commify(d$grouping$groupCounts)), ")"),
            col = unlist(CONSTANTS$plotlyColors[1:d$grouping$nGroups]),
            bty = "n"
        )
        stopSpinner(session)
    }
)

# return the plot
plot 
#----------------------------------------------------------------------
}
#----------------------------------------------------------------------
