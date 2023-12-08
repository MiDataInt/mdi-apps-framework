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
    legendTitle = "", # header for the color legend
    h = NULL, # Y-axis values at which to place line rules
    v = NULL, # X-axis values at which to place line rules
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
isReversedGroups <- xySettings$Reverse_Group_Order$value
alpha <- xySettings$Color_Alpha$value
plotAs <- plotAs[1]
#----------------------------------------------------------------------
# prepare the data groups
hasGroupingCols <- !is.null(groupingCols)
if(hasGroupingCols){
    groups <- apply(dt[, .SD, .SDcols = groupingCols], 1, paste, collapse = ", ")
    dt[, group__ := groups]
    groups <- unique(dt$group__)
    if(isAlphabeticalGroups) groups <- sort(groups)
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
plotGroups <- if(isReversedGroups) rev(groups) else groups # reverse the plot order, not the colors
hasGroups <- length(plotGroups) > 1
#----------------------------------------------------------------------
# add rule lines behind plot points/traces
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
    if(!hasGroups) return()
    par(xpd = TRUE)
    colors <- unlist(unname(groupColors[groups]))
    args <- list(
        xlim[2] * 1.1, 
        ylim[2],         
        legend = groups,
        col = colors,
        bty = "n",
        cex = 0.85,
        title = legendTitle    
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
    do.call(legend, args)
    par(xpd = FALSE)
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
