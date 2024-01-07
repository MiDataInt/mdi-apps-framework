#----------------------------------------------------------------------
# render a non-interactive XY plot with MDI formatting
#----------------------------------------------------------------------
mdiXYPlot <- function(
    plot, # the staticPlotBox we are populating, must include mdiXYPlotSettings
    dt,     # a data.table with at least columns x, y, and any groupingCols
    xlim,   # the plot X-axis limits
    ylim,   # the plot Y-axis limits     
    groupingCols = NULL, # if provided, columns in dt used to define the plotting groups
    groupColors = NULL, # if provided, a named list of colors per group
    plotAs = c("points","lines","both","area","histogram"), # how to render the XY data series
    legend_ = NULL, # optional function(groups) to modify the automated legend text
    legendTitle = "", # header for the color legend
    legendSide = c(4, 3), # side on which to plot the legend
    showLegend = TRUE, # set to FALSE to suppress the legend
    showSingleGroupLegend = FALSE, # set to TRUE to show the legend even if there is only one group plotted
    underscoresToSpaces_ = TRUE,
    hShade = NULL, # vector of two Y-axis values between which to shade the plot background
    vShade = NULL, # vector of two X-axis values between which to shade the plot background
    h = NULL, # Y-axis values at which to place line rules
    v = NULL, # X-axis values at which to place line rules
    shadeColor = "grey90", # the color used to shade the plot background
    hColor = "grey60", # the color(s) used to draw h rules
    vColor = "grey60", # the color(s) used to draw v rules
    x0Line = FALSE, # add a vertical black line at x = 0
    y0Line = FALSE, # add a horizontal black line at y = 0
    histogramSpacing = NULL # the X-axis distance alotted to histogram bars of the same X value
){ 
#----------------------------------------------------------------------
# parse the settings
plotSettings <- plot$settings # must follow the template of mdiXYPlotSettings
xySettings <- plotSettings$XY_Plot()
xJitter <- trimws(xySettings$X_Jitter_Amount$value)
yJitter <- trimws(xySettings$Y_Jitter_Amount$value)
xJitter <- if(xJitter == "") NULL else as.numeric(xJitter)
yJitter <- if(yJitter == "") NULL else as.numeric(yJitter)
isRandomPoints <- xySettings$Point_Order$value == "random"
isAlphabeticalGroups <- xySettings$Group_Order$value == "alphabetical"
isReversedGroups    <- xySettings$Reverse_Group_Order$value
isReversedPlotOrder <- xySettings$Reverse_Plot_Order$value
legendFont <- strsplit(xySettings$LegendFont$value, " ")[[1]]
alpha <- xySettings$Color_Alpha$value
plotAs <- plotAs[1]
#----------------------------------------------------------------------
# prepare the data groups
hasGroupingCols <- !is.null(groupingCols)
if(hasGroupingCols){
    groups <- apply(dt[, .SD, .SDcols = groupingCols], 1, paste, collapse = ", ") # establish groupLabels
    dt[, group__ := groups]
    groups <- unique(dt$group__)
    if(isAlphabeticalGroups) groups <- sort(groups)
    if(isReversedGroups) groups <- rev(groups)
    if(is.null(groupColors)) {
        groupColors <- CONSTANTS$plotlyColors[1:length(groups)]
        names(groupColors) <- groups
    }
} else {
    dt[, group__ := "X"]
    groupColors <- list(X = if(is.null(groupColors)) CONSTANTS$plotlyColors$blue else groupColors[1])
    groups <- "X"
}
dt[, color__ := unlist(unname(groupColors[group__]))]
plotGroups <- if(isReversedPlotOrder) rev(groups) else groups # reverse the plot stack order, not the colors
hasGroups <- length(plotGroups) > 1
#----------------------------------------------------------------------
# add rule lines behind plot points/traces
if(!is.null(hShade)) rect(xlim[1], hShade[1], xlim[2], hShade[2], col = shadeColor, border = NA)
if(!is.null(vShade)) rect(vShade[1], ylim[1], vShade[2], ylim[2] * 0.975, col = shadeColor, border = NA)
if(!is.null(h)) abline(h = h, col = hColor)
if(!is.null(v)) abline(v = v, col = vColor)
if(!is.null(x0Line)) abline(v = 0, col = "black")
if(!is.null(y0Line)) abline(h = 0, col = "black")
#----------------------------------------------------------------------
# function to add data to plots
addAlpha <- function(cols){
    if(alpha >= 0 && alpha < 1) sapply(cols, addAlphaToColor, alpha)
    else cols
}
addPoints <- function(dt, col = NULL, typ = NULL) {
    args <- list(
        x = if(is.null(xJitter)) dt$x else jitter(dt$x, a = xJitter),
        y = if(is.null(yJitter)) dt$y else jitter(dt$y, a = yJitter)
    )
    args$col <- addAlpha(if(is.null(col)) dt$color__ else col)
    if(!is.null(typ)) args <- c(args, list(
        typ = typ,
        lwd = plotSettings$get("Points_and_Lines", "Line_Width")
    ))
    do.call(plot$addPoints, args)
}
addLines <- function(dt, col) plot$addLines(  
    x = dt$x,
    y = dt$y,
    col = addAlpha(col)
)
addBoth <- function(dt, col) plot$addBoth(  
    x = dt$x,
    y = dt$y,
    col = addAlpha(col)
)
addArea <- function(dt, col) plot$addArea(
    x = dt$x,
    y = dt$y,
    col = addAlpha(col),
    border = "grey20"
)
#----------------------------------------------------------------------
# initialize the legend
addLegend <- function(points = FALSE, lines = FALSE, fill = FALSE){
    if(!showLegend) return()
    if(!hasGroups && !showSingleGroupLegend) return()
    colors <- unlist(unname(groupColors[groups]))
    if(legendSide[1] == 3){
        x <- mean(xlim)
        y <- ylim[2]
        xjust <- 0.5
        yjust <- 0
    } else { # only support top and right-side legends
        x <- xlim[2] * 1.1
        y <- ylim[2]
        xjust <- 0
        yjust <- 1
    }
    legend_ <- if(is.function(legend_)) legend_(groups) else groups
    args <- list(
        x, 
        y,
        xjust = xjust,
        yjust = yjust,
        legend = if(underscoresToSpaces_) underscoresToSpaces(legend_) else legend_,
        col = colors,
        bty = "n",
        cex = if(is.na(legendFont[2])) 0.85 else 0.95,
        title = legendTitle,
        text.font = if(legendFont[1] == "mono") 2 else 1
    )
    if(points) args <- c(args, list(
        pch    = plotSettings$get("Points_and_Lines", "Point_Type"),  
        pt.cex = plotSettings$get("Points_and_Lines", "Point_Size") * 1.5
    ))
    if(lines) args <- c(args, list(
        lty = plotSettings$get("Points_and_Lines", "Line_Type"),
        lwd = plotSettings$get("Points_and_Lines", "Line_Width")
    ))
    if(fill) args <- c(args, list(
        fill = colors,
        border = "grey20"
    ))
    par_ <- par(xpd = TRUE, family = legendFont[1])
    do.call(legend, args)
    par(par_)
}
#----------------------------------------------------------------------
# add points ...
if(plotAs == "points"){  
    if(isRandomPoints) addPoints(dt[sample(.N)]) 
    else for(group_ in plotGroups) addPoints(dt[group__ == group_])
    addLegend(points = TRUE)
#----------------------------------------------------------------------
# ... or line traces
} else if(plotAs == "lines"){
    for(group_ in plotGroups) addLines(dt[group__ == group_][order(x)], groupColors[[group_]])
    addLegend(lines = TRUE)
#----------------------------------------------------------------------
# ... or both
} else if(plotAs == "both"){
    for(group_ in plotGroups) addBoth(dt[group__ == group_][order(x)], groupColors[[group_]])
    addLegend(points = TRUE, lines = TRUE)
#----------------------------------------------------------------------
# ... or areas
} else if(plotAs == "area"){    
    for(group_ in plotGroups) addArea(dt[group__ == group_][order(x)], groupColors[[group_]])
    addLegend(fill = TRUE)
#----------------------------------------------------------------------
# ... or histograms
} else if(plotAs == "histogram"){
    nGroups <- length(groups)
    isSpaced <- !is.null(histogramSpacing) && nGroups > 1
    if(isSpaced) {
        workingNGroups <- nGroups + 1 # thus, one blank line space between each group
        spacePerGroup <- histogramSpacing / workingNGroups
        evenNLeftShift <- if(nGroups %% 2 == 0) spacePerGroup / 2 else 0 # so, if two bars, they plot on either side of the actual value
        clusterLeftShift <- floor((nGroups - 1) / 2) * spacePerGroup       
    }   
    for(i in 1:nGroups) {
        group_ <- plotGroups[i]
        dt_ <- dt[group__ == group_]
        if(isSpaced) dt_[, x := x - evenNLeftShift - clusterLeftShift + (i - 1) * spacePerGroup]  
        addPoints(dt_, groupColors[[group_]], "h")
    }
    addLegend(lines = TRUE)
}
#----------------------------------------------------------------------
}
