#----------------------------------------------------------------------
# reactive components to an interactive horizontal or vertical barplot using plot_ly
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
interactiveBarplotServer <- function(
    id, # identifier for this plot
    plotData, # a reactive that returns a df/dt with columns 'value', 'group' and 'subgroup', a named vector of values, or a named list of such objects # nolint
    shareAxis = list(), # x=TRUE or y=TRUE (not both) will cause >1 datasets to share that axis
    shareMargin = 0, # space between stacked plots
#----------------------------------------------------------------------
    orientation = 'vertical', # the orientation of the plotted bars, either horizontal or vertical
    range = NULL,  # override automatic range for the one numeric axis
#----------------------------------------------------------------------
    xtitle = NULL, # axis labels, character vector or a reactive
    ytitle = NULL,
#----------------------------------------------------------------------
    subgroupColors = NULL, #'grey', # a color name or color palette used to color the subgroupings within each group
#----------------------------------------------------------------------
    lines = NULL, # a function, reactive or vector of axis values on the numeric axis where rules are drawn, or "mean" or "median" # nolint
    lineWidth = 2,
#----------------------------------------------------------------------
    clickable  = FALSE # whether plot should react to bar clicks
#----------------------------------------------------------------------
) { moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        module <- 'interactiveBarplot' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize module
#----------------------------------------------------------------------
plotId <- ns('plot')
clicked <- reactiveVal()

#----------------------------------------------------------------------
# initialize default interaction tools and state
#----------------------------------------------------------------------
source <- if(clickable) plotId else NULL
modeBarButtons <- list( # tools available to the user
    list('zoom2d', 'pan2d'), 
    list('autoScale2d', 'resetScale2d'),
    list('toggleSpikelines'),
    list('toImage')       
)

#----------------------------------------------------------------------
# main expression to render the plot
#----------------------------------------------------------------------
renderExpr <- quote({

    # collect the required data
    d <- plotData()    
    req(d)
    isMultiPlot <- is.list(d) && 'subgroup' %notin% names(d)

    # collect the individual plots
    p <- if(isMultiPlot) {
        shareX <- !(is.null(shareAxis$x) || !shareAxis$x)
        shareY <- !(is.null(shareAxis$y) || !shareAxis$y)
        plotList <- lapply(names(d), function(name) {
            i <- which(names(d) == name)
            getBasePlot(name, i, d[[name]], isMultiPlot, shareX, shareY)
        })
        nPlots <- length(d)
        dims <- rep(1 / nPlots, nPlots)
        if(shareY) subplot(plotList, ncols = nPlots, shareY = TRUE, margin = shareMargin, widths = dims)
              else subplot(plotList, nrows = nPlots, shareX = TRUE, margin = shareMargin, heights = dims)
    } else {
        getBasePlot(NULL, 1, d, isMultiPlot)
    }

    # finish the common layout
    layout(
        p,
        xaxis = list(
            title = if(is.reactive(xtitle)) xtitle() else xtitle
            #,
            #tickmode = if(is.null(ticks$x)) 'auto' else 'linear',
            #tick0 = if(is.null(ticks$x)) NULL else ticks$x$tick0,
            #dtick = if(is.null(ticks$x)) NULL else ticks$x$dtick,
            #showgrid = !is.logical(grid$x) || grid$x,
            #gridcolor = if(is.logical(grid$x)) NULL else grid$x
        ),
        yaxis = list(
            title = if(is.reactive(ytitle)) ytitle() else ytitle
            #,
            #tickmode = if(is.null(ticks$y)) 'auto' else 'linear',
            #tick0 = if(is.null(ticks$y)) NULL else ticks$y$tick0,
            #dtick = if(is.null(ticks$y)) NULL else ticks$y$dtick,
            #showgrid = !is.logical(grid$y) || grid$y,
            #gridcolor = if(is.logical(grid$y)) NULL else grid$y
        )
    ) %>% config(
        displaylogo = FALSE,
        modeBarButtons = modeBarButtons
    )
})
        
#----------------------------------------------------------------------
# manage the plot cache to return the plot
#----------------------------------------------------------------------
output$plotly <- renderPlotly(renderExpr, quoted = TRUE) #%>% bindCache(id, cacheReactive(), cache="session")
#if(is.null(cacheReactive)) renderPlotly(renderExpr, quoted=TRUE) else

#----------------------------------------------------------------------
# render the primary plot
#----------------------------------------------------------------------

# one plot of one incoming data set
getBasePlot <- function(name, i, d, isMultiPlot, shareX=NULL, shareY=NULL){ 
    fig <- if(is.list(d)) getBasePlot_data_frame(name, i, d) else getBasePlot_vector(name, i, d)
    fig$p %>% layout(
        xaxis = list(
            range = fig$xrange,
            categoryarray = fig$x, # thus, maintain the same order as labels are encountered in the data
            categoryorder = "array"
            #        ,
            #showgrid = !is.logical(grid$x) || grid$x,
            #gridcolor = if(is.logical(grid$x)) NULL else grid$x,
            #zeroline = xzeroline
        ),
        yaxis = list(
            range = fig$yrange,
            categoryarray = fig$y,
            categoryorder = "array"
            #        ,
            #showgrid = !is.logical(grid$y) || grid$y,
            #gridcolor = if(is.logical(grid$y)) NULL else grid$y,
            #zeroline = yzeroline
        ) 
    ) %>% 
    addLines(d)
}
getRange <- function(d, i){
    if(is.reactive(range)) range()
    else if(is.function(range)) range(d, i)
    else if(length(range) > 1) range[i]
    else range
}
getBasePlot_vector <- function(name, i, d){ # no subgroups, caller provided a named vector
    fig <- if(orientation == "vertical") list(
        x = names(d),
        y = d,
        xrange = NULL,
        yrange = getRange(d, i)
    ) else list(
        x = d,
        y = names(d),
        xrange = getRange(d, i),
        yrange = NULL
    )
    fig$p <- plot_ly(
        type = "bar",
        x = fig$x,
        y = fig$y,
        source = source,
        name = name    
    )
    fig
}
getBasePlot_data_frame <- function(name, i, d){ # groups + subgroups, caller provided a data frame
    fig <- if(orientation == "vertical") list(
        x = d$group,
        y = d$value,
        xrange = NULL,
        yrange = getRange(d$value, i)
    ) else list(
        x = d$value,
        y = d$group,
        xrange = getRange(d$value, i),
        yrange = NULL
    )
    fig$p <- plot_ly(
        type = "bar",
        x = fig$x,
        y = fig$y,
        color = factor(d$subgroup),
        colors = subgroupColors,
        source = source,
        name = name    
    )
    fig
}

#----------------------------------------------------------------------
# helper functions to add elements to ploty_ly
#----------------------------------------------------------------------

# rule(s) overplotted on top of data
getLines <- function(lines, d) {
    if(is.null(lines)) NULL
    else if(is.character(lines)) get(lines)(d, na.rm = TRUE)
    else if(is.reactive(lines)) lines()
    else if(is.function(lines)) lines(d)
    else lines
}
addLines <- function(p, d){
    if(is.list(d)) d <- d$value
    Ls <- getLines(lines, d)
    if(is.null(Ls)) return(p)
    color <- if(is.null(attributes(Ls)$color)) 'black' else attributes(Ls)$color
    for(i in seq_along(Ls)) p <- {
        if(orientation == "vertical"){
            x    <- names(d)[1]
            xend <- names(d)[length(d)]
            y    <- Ls[i]
            yend <- Ls[i]
        } else {
            x    <- Ls[i]
            xend <- Ls[i]
            y    <- names(d)[1]
            yend <- names(d)[length(d)]
        }
        add_segments(
            p,
            x = x,
            xend = xend,
            y = y,
            yend = yend,
            opacity = 0.75,
            line = list(color = color[i], width = lineWidth, dash = 'solid'),
            showlegend = FALSE
        )
    }
    p
}

#----------------------------------------------------------------------
# respond to a point click by passing the event on to our caller
#----------------------------------------------------------------------
if(clickable) observe({
    req(plotData()) # suppress a warning before data exist, see: https://github.com/ropensci/plotly/issues/1538#issuecomment-495312022 # nolint
    d <- event_data("plotly_click", source = plotId)
    req(d)
    clicked(d)
})

#----------------------------------------------------------------------
# set return values as reactives that will be assigned to app$data[[stepName]]
#----------------------------------------------------------------------
list(
    clicked = clicked
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
