#----------------------------------------------------------------------
# static components for an interactive, WYSIWYG, plot
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
mdiInteractivePlotServer <- function(
    id,   
    #------------------------------
    hover = TRUE,    
    click = TRUE,
    brush = TRUE,
    delay = 500,
    #------------------------------
    contents = NULL # a reactive that returns:
    # contents = reactive({ list(
    #     pngFile = path, # OR plotArgs
    #     plotArgs = list(
    #          ... # first element must have a "plot.class" method and be named "x" or unnamed
    #     ),
    #     layout = list(
    #         width = pixels,
    #         height = pixels,
    #         pointsize = integer, # defaults to 8
    #         dpi = integer, # defaults to 96
    #         mai = integer vector,
    #         xlim = range, # OR can be read from plotArgs
    #         ylim = range
    #     ),
    #     abline = list(), # optional named arguments passed to abline() after calling plot(plotArgs)
    #     parseLayout = function(x, y) list(x, y, layout) # to convert to plot space in a multi-plot layout
    # ) })
){ moduleServer(id, function(input, output, session) {   
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize module
#----------------------------------------------------------------------
module <- 'mdiInteractivePlot' # for reportProgress tracing
fileName <- paste0(app$NAME, "-", id, "-mdiInteractivePlot.png")     
pngFile <- file.path(sessionDirectory, fileName)
idPrefix <- session$ns("")
mdiInteractivePlotInit <- observe({
    session$sendCustomMessage("mdiInteractivePlotInit", list(
        prefix = idPrefix,
        hover  = hover,    
        click  = click,
        brush  = brush,
        delay  = delay
    ))
    mdiInteractivePlotInit$destroy()
})

#----------------------------------------------------------------------
# function for reactively updating the plot or image
#----------------------------------------------------------------------
observe({
    d <- contents()
    if(is.null(d)){
        session$sendCustomMessage("mdiInteractivePlotUpdate", list(
            prefix = idPrefix,
            src = "",
            width  = 0,
            height = 0
        ))
        return()
    }
    if(is.null(d$pngFile)){
        png(
            pngFile,
            width  = d$layout$width,
            height = d$layout$height,
            units  = "px",
            pointsize = if(is.null(d$layout$pointsize)) 8 else d$layout$pointsize,
            res = if(is.null(d$layout$dpi)) 96 else d$layout$dpi,
            type = "cairo"
        )
        tryCatch({
            par(mai = d$layout$mai)
            if(is.null(d$plotArgs$xlim) && is.null(d$plotArgs$ylim)){
                do.call("plot", c(d$plotArgs, list(xlim = d$layout$xlim, ylim = d$layout$ylim)))
            } else {
                do.call("plot", d$plotArgs)
            }
            if(!is.null(d$abline)) do.call(abline, d$abline)
            dev.off()
        }, error = function(e){
            dev.off() # close device if use plot function throws error, e.g., via req(FALSE)
            req(FALSE)
        })
        d$pngFile <- pngFile
    }
    png <- if(file.exists(d$pngFile)){
        RCurl::base64Encode(readBin(d$pngFile, "raw", file.info(d$pngFile)[1, "size"]), "txt")
    } else {
        "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8Xw8AAoMBgDTD2qgAAAAASUVORK5CYII=" # 1x1 transparent pixel
    }
    session$sendCustomMessage("mdiInteractivePlotUpdate", list(
        prefix = idPrefix,
        src = sprintf('data:image/png;base64,%s', png),
        width  = d$layout$width,
        height = d$layout$height
    ))
})

#----------------------------------------------------------------------
# function for converting pixels to plot coordinates
#----------------------------------------------------------------------
pixelToAxis <- function(x, layout, leftI, rightI, dim, lim, invert){
    if(is.null(x)) return(x)
    dim <- layout[[dim]]
    lim <- layout[[lim]]
    if(invert) x <- dim - x
    dpi <- if(is.null(layout$dpi)) 96 else layout$dpi
    leftMargin  <- layout$mai[leftI]  * dpi # or bottom...
    rightMargin <- layout$mai[rightI] * dpi
    totalMargin <- leftMargin + rightMargin
    min <- leftMargin
    max <- dim - rightMargin
    if(x < min) x <- min
    if(x > max) x <- max
    (x - leftMargin) / (dim - totalMargin) * as.numeric(lim[2] - lim[1]) + lim[1] # as.numeric accounts for integer64 axes
}
pixelToAxes <- function(x, y){
    contents <- contents()
    req(contents)
    if(!is.null(contents$parseLayout)){ # multi-plot layout images
        d <- contents$parseLayout(x, y)
        x <- d$x
        y <- d$y
        layout <- d$layout 
    } else if(!is.null(contents$layout)){ # single-plot images
        layout <- contents$layout
    } else {
        return(list(x = x, y = y))
    }
    if(is.null(layout$xlim)) layout$xlim <- contents$plotArgs$xlim
    if(is.null(layout$ylim)) layout$ylim <- contents$plotArgs$ylim
    req(layout$xlim)
    req(layout$ylim)
    req(layout$mai)
    list(
        x = pixelToAxis(x, layout, 2, 4, "width",  "xlim", FALSE), 
        y = pixelToAxis(y, layout, 1, 3, "height", "ylim", TRUE)
    )
}

#----------------------------------------------------------------------
# react to user interactions with plot
#----------------------------------------------------------------------
parseEvent <- function(d = NULL, x = NULL, y = NULL, keys = list()){
    isolate({ # in case parseLayout() calls a reactive (we are reacting to the plot interaction)
        if(is.null(d)) d <- list(
            coord = list(
                x = x,
                y = y
            ),
            keys = keys
        )
        list(
            coord = pixelToAxes(d$coord$x, d$coord$y), 
            keys = d$keys
        )        
    })
}
hover <- if(hover) reactive({
    req(input$hover)
    parseEvent(input$hover)
}) else NULL
click <- if(click) reactive({
    req(input$click, input$click$coord, input$click$keys)
    parseEvent(input$click)
}) else NULL
brush <- if(brush) reactive({
    req(input$brush)
    d <- input$brush
    d1 <- parseEvent(x = d$coord$x1, y = d$coord$y1)
    d2 <- parseEvent(x = d$coord$x2, y = d$coord$y2)
    list(
        coord = list(
            x1 = d1$coord$x,
            y1 = d1$coord$y,
            x2 = d2$coord$x,
            y2 = d2$coord$y
        ),
        keys = input$brush$keys
    )
}) else NULL

#----------------------------------------------------------------------
# set return values as reactives that will be assigned to app$data[[stepName]]
#----------------------------------------------------------------------
list(
    hover = hover,    
    click = click,
    brush = brush,
    pixelToAxes = pixelToAxes,
    pixelToAxis = pixelToAxis
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
