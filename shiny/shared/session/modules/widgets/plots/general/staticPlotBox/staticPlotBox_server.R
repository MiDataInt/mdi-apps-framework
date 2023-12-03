#----------------------------------------------------------------------
# reactive components for a non-interactive, WYSIWYG, publication ready plot
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
staticPlotBoxServer <- function(
    id,
    #----------------------------
    create = function() NULL, # a function or reactive that creates the plot
    maxHeight = "400px",
    points  = FALSE, # set to TRUE to expose relevant plot options
    lines   = FALSE,
    legend  = FALSE,
    margins = FALSE,
    title   = FALSE,
    #---------------------------- passed to activateMdiHeaderLinks
    url = getDocumentationUrl(
        "shiny/shared/session/modules/widgets/plots/staticPlotBox/README", 
        framework = TRUE
    ),
    baseDirs = NULL, # in addition to plots/staticPlotBox
    envir = parent.frame(),
    dir = NULL,
    settings = NULL, # an additional settings template as a list()
    template = settings, # for legacy support
    Plot_Frame = NULL, # a reactive that provides overrides that take precedence over settings$Plot_Frame, e.g., reactive(list(Width_Inches = 7))
    ... # additional arguments passed to settingsServer
){ moduleServer(id, function(input, output, session) {   
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize module
#----------------------------------------------------------------------
module <- 'staticPlotBox' # for reportProgress tracing
plotId <- session$ns('plot')
pngFileName <- paste(plotId, "png", sep = ".")
pngFile <- file.path(sessionDirectory, pngFileName)

#----------------------------------------------------------------------
# parse requested plot options
#----------------------------------------------------------------------
settingsFile <- file.path(serverEnv$SHARED_DIR, 'session', 'modules', 'widgets', 'plots', 'general', 'staticPlotBox', 'settings.yml') # nolint
callerTemplate <- template
template <- read_yaml(settingsFile)
if(points || lines){
    if(!points) {
        template$Points_and_Lines$Point_Size <- NULL
        template$Points_and_Lines$Point_Type <- NULL
    }
    if(!lines){
        template$Points_and_Lines$Line_Width <- NULL
        template$Points_and_Lines$Line_Type <- NULL
    }
} else {
    template$Points_and_Lines <- NULL
}
if(!margins){
    template$Plot_Frame$Top_Margin <- NULL
    template$Plot_Frame$Bottom_Margin <- NULL
    template$Plot_Frame$Left_Margin <- NULL
    template$Plot_Frame$Right_Margin <- NULL
}
if(!legend) template$Plot_Frame$Legend_Placement$type <- "spacer"
if(!title) template$Plot_Frame$Title <- NULL
if(!is.null(callerTemplate)) template <- c(template, callerTemplate)
if(!is.null(Plot_Frame)){ # mask overrides from settings inputs
    tmp <- Plot_Frame()
    for(x in names(tmp)) template$Plot_Frame[[x]] <- NULL
}
getPlotFrameOverrides <- function(){ # get the current value of masked overrides
    if(is.null(Plot_Frame)) return(NULL)
    tmp <- Plot_Frame()
    pf <- list()
    for(x in names(tmp)) pf[[x]] <- list(value = tmp[[x]])
    pf
}

#----------------------------------------------------------------------
# activate all requested/required header links
#----------------------------------------------------------------------
settings <- activateMdiHeaderLinks(
    session,
    #----------------------------
    url = url, # documentation
    reload = function() invalidatePlot( sample(1e8, 1) ), # reload
    baseDirs = unique(c( # code editor
        baseDirs,
        getWidgetDir("plots/general/staticPlotBox", framework = TRUE)
    )),
    envir = envir, # R console
    dir = dir, # terminal emulator
    download = downloadHandler( # plot download
        filename = pngFileName,
        content = function(tmpFile) file.copy(pngFile, tmpFile),
        contentType = "image/png"
    ),
    #----------------------------
    settings = module, # plot settings
    templates = list(template),
    fade = FALSE,
    title = "Plot Parameters",
    ...
)

#----------------------------------------------------------------------
# render the plot as a static image filled by caller
#----------------------------------------------------------------------
invalidatePlot <- reactiveVal(NULL)
output$plot <- renderImage({
    invalidatePlot()
    ps <- c(settings$Plot_Frame(), getPlotFrameOverrides())
    
    # initialize plot
    png(
        pngFile, 
        width     = ps$Width_Inches$value, 
        height    = ps$Height_Inches$value, 
        pointsize = ps$Font_Size$value, 
        units = "in",
        res = 600,
        type = "cairo"
    )

    # let caller create the plot
    tryCatch({
        create()
        graphics.off()
    }, error = function(e){
        graphics.off()
        print(e)
        req(FALSE)
    })

    # finish plot and return as image
    list(
        src = pngFile,
        width = "100%",
        style = paste0("max-height: ", maxHeight, "; object-fit: contain;")
    )
}, deleteFile = FALSE)

#----------------------------------------------------------------------
# helper functions for constructing plots based on box settings
#----------------------------------------------------------------------
setMargins <- function() par(mar = c(
    settings$get("Plot_Frame", "Bottom_Margin"), 
    settings$get("Plot_Frame", "Left_Margin"), 
    settings$get("Plot_Frame", "Top_Margin"), 
    settings$get("Plot_Frame", "Right_Margin")
))
initializeFrame <- function(title = NULL, ...){
    ps <- c(settings$Plot_Frame(), getPlotFrameOverrides())
    if(margins) setMargins()
    plot(
        NA, 
        NA, 
        typ = "n",
        main = if(!is.null(title)) title else settings$get("Plot_Frame", "Title"),
        cex.main = (ps$Font_Size$value + 0.5) / ps$Font_Size$value,
        ...
    )
}
addPoints <- function(pch = NULL, cex = NULL, ...){
    points(
        pch = if(!is.null(pch)) pch else if(points) settings$get("Points_and_Lines", "Point_Type") else 19,
        cex = if(!is.null(cex)) cex else if(points) settings$get("Points_and_Lines", "Point_Size") else 1,
        ...
    )
}
addLines <- function(lty = NULL, lwd = NULL, ...){
    lines(
        lty = if(!is.null(lty)) lty else if(lines) settings$get("Points_and_Lines", "Line_Type")  else 1,        
        lwd = if(!is.null(lwd)) lwd else if(lines) settings$get("Points_and_Lines", "Line_Width") else 1,
        ...
    )
}
addBoth <- function(pch = NULL, cex = NULL, lty = NULL, lwd = NULL, ...){
    points(
        pch = if(!is.null(pch)) pch else if(points) settings$get("Points_and_Lines", "Point_Type") else 19,
        cex = if(!is.null(cex)) cex else if(points) settings$get("Points_and_Lines", "Point_Size") else 1,
        lty = if(!is.null(lty)) lty else if(lines) settings$get("Points_and_Lines", "Line_Type")  else 1,        
        lwd = if(!is.null(lwd)) lwd else if(lines) settings$get("Points_and_Lines", "Line_Width") else 1,
        typ = "b",
        ...
    )
}
addArea <- function(x, y, ...){
    polygon(
        c(x[1], x, x[length(x)]), 
        c(0, y, 0), 
        ...
    )
}
addLegend <- function(pch = NULL, pt.cex = NULL, lty = NULL, lwd = NULL, ...){
    placement <- if(legend) settings$get('Plot_Frame', 'Legend_Placement') else "topleft"
    tryCatch(if(placement != "none") legend(
        placement,
        pch    = if(!is.null(pch))    pch    else if(points) settings$get("Points_and_Lines", "Point_Type") else NA,
        pt.cex = if(!is.null(pt.cex)) pt.cex else if(points) settings$get("Points_and_Lines", "Point_Size") else NA,
        lty    = if(!is.null(lty))    lty    else if(lines)  settings$get("Points_and_Lines", "Line_Type")  else NA,
        lwd    = if(!is.null(lwd))    lwd    else if(lines)  settings$get("Points_and_Lines", "Line_Width") else NA,
        ...
    ), error = function(e) print(e))
}
addMarginLegend <- function(x, y, pch = NULL, pt.cex = NULL, lty = NULL, lwd = NULL, ...){
    par(xpd = TRUE)
    tryCatch(legend(
        x = x,
        y= y,
        pch    = if(!is.null(pch))    pch    else if(points) settings$get("Points_and_Lines", "Point_Type") else NA,
        pt.cex = if(!is.null(pt.cex)) pt.cex else if(points) settings$get("Points_and_Lines", "Point_Size") else NA,
        lty    = if(!is.null(lty))    lty    else if(lines)  settings$get("Points_and_Lines", "Line_Type")  else NA,
        lwd    = if(!is.null(lwd))    lwd    else if(lines)  settings$get("Points_and_Lines", "Line_Width") else NA,
        ...
    ), error = function(e) print(e))
    par(xpd = FALSE)
}

#----------------------------------------------------------------------
# set return values as reactives that will be assigned to app$data[[stepName]]
#----------------------------------------------------------------------
list(
    settings        = settings,
    get             = settings$get,
    setMargins      = setMargins,
    initializeFrame = initializeFrame,
    addPoints       = addPoints,
    addLines        = addLines,
    addBoth         = addBoth,
    addArea         = addArea,
    addLegend       = addLegend,
    addMarginLegend = addMarginLegend
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
