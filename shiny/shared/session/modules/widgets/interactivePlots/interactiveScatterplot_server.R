#----------------------------------------------------------------------
# reactive components to an interactive XY scatter plot using plot_ly
# https://plotly.com/r/
# https://github.com/plotly/plotly.js/blob/master/src/components/modebar/buttons.js
# https://github.com/plotly/plotly.js/blob/master/src/plot_api/plot_config.js
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
interactiveScatterplotServer <- function(
    id, # identifier for this plot
    plotData, # data to plot, a reactive that returns df/dt with $x and $y, or a named list of such df/dt
    accelerate = FALSE, # if TRUE, use scattergl/WebGL (instead of SVG) to plot large data series much faster (with limitations) # nolint
    shareAxis = list(), # x=TRUE or y=TRUE (not both) will cause >1 datasets to share that axis; incompatible with overplotting or fitting # nolint
    shareMargin = 0, # space between stacked plots
#----------------------------------------------------------------------
    mode = "markers", # how to plot; markers, lines, etc.
    color = NA,  # NA is auto (not NULL)
    symbol = NA, # symbol and pointSize can also be provided as columns in plotData()    
    pointSize = 3,
    lineWidth = 2,
#----------------------------------------------------------------------
    overplot = NULL,     # repeated or extra points plotted on top of the original points; if character, column of that name becomes the trace number # nolint
    overplotMode = NULL, #     if plotData is a list of df/dt, overplot must be NULL or an equal length list with the same names # nolint
    overplotColor = NA,  # defaults to the same mode as the main plot
    overplotPointSize = 3, 
    overplotLineWidth = 2,
#----------------------------------------------------------------------
    xtitle = "x", # axis labels
    xrange = NULL, # override automatic x and y axis limits
    xzeroline = TRUE,
    ytitle = "y",
    yrange = NULL,
    yzeroline = TRUE,
#----------------------------------------------------------------------
    ticks = list(x = NULL,y = NULL), # either NULL (default) or list(tick0=#, dtick=#) for the x (vertical) and y (horizontal) grids  # nolint
    grid = list(x = TRUE, y = TRUE), # either TRUE (default), FALSE (omitted), or a color value for the x (vertical) and y (horizontal) grids # nolint
#----------------------------------------------------------------------
    selectable = FALSE, # whether point selection is enabled; either FALSE, TRUE (defaults to box select), 'select' (same as TRUE), 'lasso', 'h', or 'v' # nolint
    clickable  = FALSE, # whether plot should react to point clicks
    keyColumn = NULL, # the name of the column in plotData to add as a click/select key; ends up in 'customdata' field of event_data # nolint
#----------------------------------------------------------------------
    hoverText = NULL, # character vector with hover text, or a function or reactive that returns one; if a 1-length character vector, hoverText taken from that column # nolint
    labelCol = NULL, # the name of a column from which to read the text labels applied to a subset of points (use NA for unlabeled points) # nolint
    labelDirs = list(x = 1, y = 1), # direction to draw the label arrow relative to x,y; 0=no offset, 1=farther along the axis, -1=opposite of 1 (i.e, to the inside) # nolint
#----------------------------------------------------------------------
    fitMethod = NULL, # a reactive that supplies a fit, a function(d) that returns a fit, or a method compatible with fitTrendline # nolint
    fitColor = NA,
#----------------------------------------------------------------------
    unityLine = FALSE, # add a unity line after plotting the points
    hLines = NULL, # a function, reactive or vector of axis values
    vLines = NULL,
#---------------------------------------------------------------------- 
    distributions = NULL, # a function that returns a list of df/dt with $x and $y to plot as individual grey, dashed line distribution traces # nolint
#----------------------------------------------------------------------
    cacheReactive = NULL # optional reactive with (hopefully simple to parse) values on which the plot depends; passed to bindCache as cache keys # nolint
#----------------------------------------------------------------------
) { moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        module <- 'interactiveScatterplot' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize module
#----------------------------------------------------------------------
plotId <- ns('plot')
scatterType <- if(accelerate) 'scattergl' else 'scatter'
selected <- reactiveVal()
clicked <- reactiveVal()
fit <- reactiveVal()

#----------------------------------------------------------------------
# initialize default interaction tools and state
#----------------------------------------------------------------------
if(is.logical(selectable)){
    selectmode <- 'select'
    selectdirection <- NULL
} else if(selectable == 'h' || selectable == 'v'){
    selectmode <- 'select'
    selectdirection <- selectable    
    selectable <- TRUE
} else {
    selectmode <- selectable
    selectdirection <- NULL    
    selectable <- TRUE
}
source <- if(selectable || clickable) plotId else NULL
dragmode <- if(selectable) selectmode else 'zoom' # or lasso, pan, https://plotly.com/r/reference/layout/#layout-dragmode # nolint
modeBarButtons <- list( # tools available to the user
    list('zoom2d', 'pan2d'), 
    list('autoScale2d', 'resetScale2d'),
    list('toggleSpikelines'),
    list('toImage')       
)
selectButtons <- list(
    list('select2d', 'lasso2d')
)
if(selectable) modeBarButtons <- c(selectButtons, modeBarButtons)
if(is.null(overplotMode)) overplotMode <- mode

#----------------------------------------------------------------------
# styling helper functions
#----------------------------------------------------------------------
getStyle <- function(d, type, default) if(type %in% names(d)) d[[type]] else default
getPointSize <- function(d, default) getStyle(d, 'pointSize', default)
getSymbol    <- function(d, default) getStyle(d, 'symbol',    default)
setHoverText <- function(d) {
    if(is.data.frame(d)) d$hoverText_ <-
        if(is.null(hoverText)) NA
        else if(is.character(hoverText) &&
                length(hoverText) == 1 &&
                nrow(d) > 1) d[[hoverText]]
        else if(is.reactive(hoverText)) hoverText() # expected to yield a final character vector of same length as data
        else if(is.function(hoverText)) hoverText(d)
        else hoverText
    d
}
getOverplot <- function(d, overplot, name=NULL){
    if(is.character(overplot) && is.data.frame(d)){ # split a data frame into groups by column overplot
        traceIs <- unique(d[[overplot]])
        traceIs <- traceIs[traceIs > 0]
        if(length(traceIs) == 0) return(d[FALSE, ])
        lapply(1:max(traceIs, na.rm = TRUE), function(traceI) { # allows blank traces to control point coloring
            rows <- d[[overplot]] == traceI
            d[rows, ] # do not use d[d[[overplot]] == traceI,] it fails for data.tables
        })
    } else if(is.reactive(overplot) && is.null(name)) overplot()  # functions and reactives are only applied at the top level, not for each multiplot # nolint
      else if(is.function(overplot) && is.null(name)) overplot(d)
      else if(!is.null(overplot) && !is.null(name)) overplot[[name]] # here, overplot was previously returned by function/reactive above # nolint
      else overplot
}

#----------------------------------------------------------------------
# main expression to render the plot
#----------------------------------------------------------------------
renderExpr <- quote({
    
    # collect the required data - BEFORE startSpinner, since plotData calls may use req()
    # if plotData is slow, caller is expected to run the spinner for that portion
    d <- plotData() # expected to yield a df/dt, or a names list of them
    req(d)
    if (!is.null(cacheReactive)) startSpinner(session, id)
    isMultiPlot <- !is.data.frame(d)
    d <- setHoverText(d)
    overplot_ <- getOverplot(d, overplot)
    if(is.character(overplot) && !isMultiPlot) d <- d[d[[overplot]] == 0, ]      

    # collect the individual plots
    p <- if(isMultiPlot) {
        shareX <- !(is.null(shareAxis$x) || !shareAxis$x)
        shareY <- !(is.null(shareAxis$y) || !shareAxis$y)
        plotList <- lapply(names(d), function(name) {         
            d_ <- setHoverText(d[[name]])
            overplot__ <- getOverplot(d_, overplot_, name)
            if(is.character(overplot_)) d_ <- d_[d_[[overplot_]] == 0, ] 
            getBasePlot(name, d_, overplot__, isMultiPlot, shareX, shareY)
        })
        nPlots <- length(d)
        dims <- rep(1 / nPlots, nPlots)
        if(shareY) subplot(plotList, ncols = nPlots, shareY = TRUE, margin = shareMargin, widths = dims)
              else subplot(plotList, nrows = nPlots, shareX = TRUE, margin = shareMargin, heights = dims)
    } else {
        getBasePlot(NULL, d, overplot_, isMultiPlot)
    }

    # finish the common layout
    p <- layout(
        p,
        xaxis = list(
            title = getTitle(xtitle),
            zeroline = xzeroline,
            tickmode = if(is.null(ticks$x)) 'auto' else 'linear',
            tick0 = if(is.null(ticks$x)) NULL else ticks$x$tick0,
            dtick = if(is.null(ticks$x)) NULL else ticks$x$dtick,
            showgrid = !is.logical(grid$x) || grid$x,
            gridcolor = if(is.logical(grid$x)) NULL else grid$x
        ),
        yaxis = list(
            title = getTitle(ytitle),
            zeroline = yzeroline,
            tickmode = if(is.null(ticks$y)) 'auto' else 'linear',
            tick0 = if(is.null(ticks$y)) NULL else ticks$y$tick0,
            dtick = if(is.null(ticks$y)) NULL else ticks$y$dtick,
            showgrid = !is.logical(grid$y) || grid$y,
            gridcolor = if(is.logical(grid$y)) NULL else grid$y
        ),
        dragmode = dragmode,
        selectdirection = selectdirection
    ) %>% config(
        displaylogo = FALSE,
        modeBarButtons = modeBarButtons
    )

    if (!is.null(cacheReactive)) stopSpinner(session, id)
    p
})
                       
#----------------------------------------------------------------------
# manage the plot cache to return the plot
#----------------------------------------------------------------------
output$plotly <- if(is.null(cacheReactive)) renderPlotly(renderExpr, quoted = TRUE) 
                 else renderPlotly(renderExpr, quoted = TRUE) %>% bindCache(id, cacheReactive(), cache = "session")
           
#----------------------------------------------------------------------
# render the primary plot
#----------------------------------------------------------------------

# one plot of one incoming data set
getBasePlot <- function(name, d, overplot, isMultiPlot, shareX = NULL, shareY = NULL){
    d <- d[!is.na(x) & !is.na(y)]
    if(is.null(overplot)) overplot <- d[FALSE, ]
    
    # one plot of one incoming data set
    p <- plot_ly(
        d,
        x = ~x,
        y = ~y,
        type = scatterType,
        mode = mode,
        source = source,
        name = name,
        marker = if(grepl('markers', mode)) list(
            size = getPointSize(d, pointSize),
            color = color,
            symbol = getSymbol(d, symbol)
        ) else NULL,
        line = if(grepl('lines', mode)) list(
            width = lineWidth,
            color = color
        ) else NULL,
        text = ~hoverText_,
        hoverinfo = if(is.null(hoverText)) NULL else 'text',
        customdata = if(is.null(keyColumn)) NA else d[[keyColumn]] 
    ) %>% layout(
        xaxis = list(
            range = if(is.reactive(xrange)) xrange()
                    else if(is.function(xrange)) xrange(d, 'x')
                    else xrange,
            showgrid = !is.logical(grid$x) || grid$x,
            gridcolor = if(is.logical(grid$x)) NULL else grid$x,
            zeroline = xzeroline
        ),
        yaxis = list(
            range = if(is.reactive(yrange)) yrange()
                    else if(is.function(yrange)) yrange(d, 'y')
                    else yrange,
            showgrid = !is.logical(grid$y) || grid$y,
            gridcolor = if(is.logical(grid$y)) NULL else grid$y,
            zeroline = yzeroline
        ) 
    )  
    
    # add overplots
    if(is.data.frame(overplot)){
        p <- addOverplot(p, overplot)
    } else {
        for(x in overplot) p <- addOverplot(p, x)
    }

    # add curve fit
    if(!isMultiPlot){
        fit <- if(selectable) fit() else getFit(d)
        p <- addFitPoints(p, d, fit) 
    }

    # add guidelines
    addUnityLine(p, d) %>%
    addHLines(d) %>%
    addVLines(d) %>%
    addDistributions(d) %>%
    addPointLabels(d)
}

#----------------------------------------------------------------------
# helper functions to add elements to ploty_ly
# TODO: update lines to use add_segments instead of add_trace?
#----------------------------------------------------------------------

# parse an axis title 
getTitle <- function(title){
    if(is.reactive(title) || is.function(title)) title() else title
}

# add one set of overplot data points
addOverplot <- function(p, overplot){
    overplot # do not delete; for unexplained reasons, multiple overplots sometimes depend on accessing overplot like this (??) # nolint
    add_trace(
        p,
        x = ~overplot$x,
        y = ~overplot$y,
        mode = overplotMode,
        marker = if(grepl('markers', overplotMode)) list(
            size = getPointSize(overplot, overplotPointSize),
            color = overplotColor
        ) else NULL,
        line = if(grepl('lines', overplotMode)) list(
            width = overplotLineWidth,
            color = overplotColor
        ) else NULL,
        showlegend = FALSE,
        text = if(is.null(overplot$hoverText_)) NA else ~overplot$hoverText_,
        hoverinfo = if(is.null(hoverText)) NULL else 'text',
        customdata = if(is.null(keyColumn)) NA else overplot[[keyColumn]] 
    )
}

# add text labels to user-specified points
addPointLabels <- function(p, d){
    if(is.null(labelCol)) return(p)
    labels <- d[!is.na(d[[labelCol]]), ]
    if(nrow(labels) == 0) return(p)
    signx <- labelDirs$x * sign(labels$x)
    signy <- labelDirs$y * sign(labels$y)
    add_annotations(
        p,
        x = labels$x,
        y = labels$y,
        xref = "x",
        yref = "y",        
        text = labels[[labelCol]],
        showarrow = TRUE,
        arrowhead = 4,
        arrowsize = 0.75,
        ax =  20 * signx,
        ay = -40 * signy,
        xshift = 2 * signx,
        yshift = 4 * signy
    )
}

# points (not a line trace) of a provide curve fit
addFitPoints <- function(p, d, fit){
    if(is.null(fit)) return(p)
    fit(fit)
    if(is.data.frame(fit)){
        add_trace(
            p,
            x = ~fit$x,
            y = ~fit$y,
            mode = 'markers',
            marker = list(
                size = getPointSize(fit, pointSize) * 1.5,
                color = fitColor
            ),
            color = NULL,
            showlegend = FALSE,
            text = NA
        )  
    } else {
        f <- predict(fit, data.frame(x = d$x, x2 = d$x^2, x3 = d$x^3))
        add_trace(
            p,
            y = ~f,
            mode = 'markers',
            marker = list(
                size = getPointSize(d, pointSize),
                color = fitColor
            ),
            color = NULL,
            showlegend = FALSE,
            text = NA
        )        
    }   
}

# a unity line to help identify deviations from identity of two data sets
addUnityLine <- function(p, d){
    if(!unityLine) return(p)
    min <- min(d$x, d$y, na.rm = TRUE)
    max <- max(d$x, d$y, na.rm = TRUE)
    add_trace(
        p,
        x = c(min, max),
        y = c(min, max),
        type = 'scatter',
        mode = 'lines',
        opacity = 0.75,
        line = list(color = 'black', width = lineWidth, dash = 'solid'),
        showlegend = FALSE,
        text = NA
    )      
}

# horizontal rule(s) overplotted on top of data
getLines <- function(lines, d) {
    if(is.null(lines)) NULL
    else if(is.reactive(lines)) lines()
    else if(is.function(lines)) lines(d)
    else lines
}
addHLines <- function(p, d){
    Ls <- getLines(hLines, d)
    if(is.null(Ls)) return(p)
    color <- if(is.null(attributes(Ls)$color)) 'black' else attributes(Ls)$color
    x <- range(d$x[d$x > -Inf & d$x < Inf], na.rm = TRUE)
    #x <- c(min(-1e9,x[1]), max(1e9,x[2]))
    for(i in seq_along(Ls)) p <- add_trace(
        p,
        x = x,
        y = c(Ls[i], Ls[i]),
        type = 'scatter',
        mode = 'lines+markers',
        opacity = 0.75,
        line = list(color = color[i], width = lineWidth, dash = 'solid'),
        marker = list(size = 0.1),
        showlegend = FALSE,
        text = NA
    )
    p
}

# vertical rule(s) overplotted on top of data
addVLines <- function(p, d){
    Ls <- getLines(vLines, d)
    if(is.null(Ls)) return(p)
    color <- if(is.null(attributes(Ls)$color)) 'black' else attributes(Ls)$color
    y <- range(d$y[d$y > -Inf & d$y < Inf], na.rm = TRUE)
    #y <- c(min(-1e9,y[1]), max(1e9,y[2]))
    for(i in seq_along(Ls)) p <- add_trace(
        p,
        x = c(Ls[i], Ls[i]),
        y = y,
        type = 'scatter',
        mode = 'lines+markers', # we don't want markers, but plotly give an inappropriate warning if we don't use invisible markers # nolint
        opacity = 0.75,
        line = list(color = color[i], width = lineWidth, dash = 'solid'),
        marker = list(size = 0.1),
        showlegend = FALSE,
        text = NA
    )
    p  
}

# grey, dashed line traces to plot data calculated distributions on top of density or histograms
addDistributions <- function(p, d){
    if(is.null(distributions)) return(p)
    distributions <- distributions(d)
    if(is.null(distributions) || length(distributions) == 0) return(p)
    for(i in seq_along(distributions)) p <- add_trace(
        p,
        x = distributions[[i]]$x,
        y = distributions[[i]]$y,
        type = 'scatter',
        mode = 'lines',
        line = list(color = "grey", dash = 'dot'),
        showlegend = FALSE,
        text = NA
    )
    p
}

#----------------------------------------------------------------------
# respond to an interactive selection event of data points by fitting a curve
#----------------------------------------------------------------------
getFit <- function(d){
    if(is.null(fitMethod)) NULL
    else if(is.reactive(fitMethod)) fitMethod()
    else if(is.function(fitMethod)) fitMethod(d)
    else fitTrendline(d, fitMethod)
}
if(selectable) observe({
    req(plotData()) # suppress a warning before data exist, see: https://github.com/ropensci/plotly/issues/1538#issuecomment-495312022 # nolint
    d <- event_data("plotly_selected", source = plotId)
    req(d)
    req(nrow(d) > 0)
    d <- d[d$curveNumber == 0, ] # to avoiding including a prior fit/trendline in the next fit
    selected(d)
    fit( getFit(d) )
})
#'data.frame':   1297 obs. of  4 variables:
# $ curveNumber: int  0 0 0 0 0 0 0 0 0 0 ... <<<<<< both indices are zero-referenced
# $ pointNumber: int  280 281 282 283 284 285 286 287 288 289 ...
# $ x          : int  281 282 283 284 285 286 287 288 289 290 ...
# $ y          : num  10059 11032 10321 11259 11702 ...

#----------------------------------------------------------------------
# respond to a point click by passing the event on to our caller
#----------------------------------------------------------------------
if(clickable) observe({
    req(plotData()) # suppress a warning before data exist, see: https://github.com/ropensci/plotly/issues/1538#issuecomment-495312022 # nolint
    d <- event_data("plotly_click", source = plotId)
    req(d)
    clicked(d)
})
#'data.frame':   1 obs. of  4 variables:
# $ curveNumber: int 1 <<<< this is the overplot, curve 0 is the first one plotted
# $ pointNumber: int 0
# $ x          : int 21
# $ y          : int 28909794

#----------------------------------------------------------------------
# set return values as reactives that will be assigned to app$data[[stepName]]
#----------------------------------------------------------------------
list(
    selected = selected,
    clicked = clicked,
    fit = fit
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# modebar buttons available as of July 2021
#----------------------------------------------------------------------
#toImage
#sendDataToCloud
#editInChartStudio 
#zoom2d 
#pan2d
#select2d
#lasso2d
#drawclosedpath
#drawopenpath
#drawline
#drawrect
#drawcircle
#eraseshape
#zoomIn2d
#zoomOut2d
#autoScale2d
#resetScale2d
#hoverClosestCartesian
#hoverCompareCartesian
#zoom3d
#pan3d
#orbitRotation
#tableRotation
#resetCameraDefault3d
#resetCameraLastSave3d
#hoverClosest3d
#zoomInGeo
#zoomOutGeo
#resetGeo
#hoverClosestGeo
#hoverClosestG12d
#hoverClosestPie
#resetViewSankey
#toggleHover
#resetViews
#toggleSpikeLines
#setSpikelineVisibility
#resetViewMapbox
#zoomInMapbox
#zoomOutMapbox
