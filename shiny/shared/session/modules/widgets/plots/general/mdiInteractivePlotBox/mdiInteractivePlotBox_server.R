#----------------------------------------------------------------------
# reactive components for a box wrapper around mdiInteractivePlot
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
mdiInteractivePlotBoxServer <- function(
    id,
    #----------------------------
    hover = FALSE, # options consumed by mdiInteractivePlotServer
    click = TRUE,
    brush = TRUE,
    #----------------------------
    create = function() NULL, # a function or reactive that creates the plot as a png file using settings and helpers
    points  = FALSE, # set to TRUE to expose relevant plot options
    lines   = FALSE,
    legend  = FALSE,
    margins = FALSE,
    title   = FALSE,
    #---------------------------- passed to activateMdiHeaderLinks
    url = getDocumentationUrl(
        "shiny/shared/session/modules/widgets/plots/mdiInteractivePlotBox/README", 
        framework = TRUE
    ),
    baseDirs = NULL, # in addition to plots/mdiInteractivePlotBox
    envir = parent.frame(),
    dir = NULL,
    data = FALSE,
    settings = NULL, # an additional settings template as a list()
    template = settings, # for legacy support
    defaults = NULL, # list of default settings values use to inialize settings
    ... # additional arguments passed to settingsServer
){ moduleServer(id, function(input, output, session) {   
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize module
#----------------------------------------------------------------------
module <- 'mdiInteractivePlotBox' # for reportProgress tracing
plotId <- session$ns('plot')
pngFileName <- paste(plotId, "png", sep = ".")
txtFileName <- paste(plotId, "txt", sep = ".")
pngFile <- file.path(sessionDirectory, pngFileName)
txtFile <- file.path(sessionDirectory, txtFileName)

#----------------------------------------------------------------------
# parse requested plot options
#----------------------------------------------------------------------
settingsFile <- file.path(serverEnv$SHARED_DIR, 'session', 'modules', 'widgets', 'plots', 'general', 'mdiInteractivePlotBox', 'settings.yml') # nolint
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
if(!is.null(defaults)) for(tab in names(defaults)) {
    for(opt in names(defaults[[tab]])) {
        template[[tab]][[opt]]$value <- defaults[[tab]][[opt]]
    }
}
if(!is.null(callerTemplate)) template <- c(template, callerTemplate)

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
        getWidgetDir("plots/general/mdiInteractivePlotBox", framework = TRUE)
    )),
    envir = envir, # R console
    dir = dir, # terminal emulator
    download = downloadHandler( # plot download
        filename = pngFileName,
        content = function(tmpFile) file.copy(pngFile, tmpFile),
        contentType = "image/png"
    ),
    data = if(data) downloadHandler( # data table download
        filename = txtFile,
        content = function(tmpFile) file.copy(txtFile, tmpFile),
        contentType = "text/plain"
    ) else NULL,
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
# invalidatePlot <- reactiveVal(NULL)
plot <- mdiInteractivePlotServer(
    'plot', 
    hover = hover,
    click = click,
    brush = brush,
    contents = reactive({ 
        list(
            pngFile = pngFile, 
            layout = tryCatch(
                create(settings),
                error = function(e){
                    unlink(pngFile)
                    NULL
                }
            )
        ) 
    })
)

#----------------------------------------------------------------------
# helper functions for constructing plots based on box settings
#----------------------------------------------------------------------
setMargins <- function() par(mar = c(
    settings$get("Plot_Frame", "Bottom_Margin"), 
    settings$get("Plot_Frame", "Left_Margin"), 
    settings$get("Plot_Frame", "Top_Margin"), 
    settings$get("Plot_Frame", "Right_Margin")
))
initializePng <- function(){
    dpi           <- 96 # resolution optimized for screen display
    width_inches  <- settings$get("Plot_Frame","Width_Inches")
    height_inches <- settings$get("Plot_Frame","Height_Inches")
    width_pixels  <- width_inches  * dpi
    height_pixels <- height_inches * dpi
    pointsize     <- settings$get("Plot_Frame","Font_Size") 
    png(file = pngFile, width = width_pixels, height = height_pixels, units = "px", 
        pointsize = pointsize, res = dpi, type = "cairo")
    if(margins) setMargins()
    list(
        width     = width_pixels,
        height    = height_pixels,
        pointsize = pointsize,
        dpi       = dpi
    )
}
initializeFrame <- function(layout, xlim, ylim, title = NULL, ...){
    layout$xlim <- xlim
    layout$ylim <- ylim
    plot(
        NA, 
        NA, 
        typ = "n",
        xlim = xlim,
        ylim = ylim,
        ...
    )
    userTitle <- settings$get("Plot_Frame","Title") %>% trimws
    title <- if(length(userTitle) == 0 || userTitle == "") title else userTitle
    if(is.null(title)) title <- ""
    mtext(
        text = title,
        side = 3,
        line = 0.5,
        cex  = 0.9
    )
    layout$mai <- par("mai")
    layout
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
finishPng <- function(layout){
    dev.off()
    stopSpinner(session)
    layout
}

#----------------------------------------------------------------------
# set return values as reactives that will be assigned to app$data[[stepName]]
#----------------------------------------------------------------------
list(
    plot            = plot,
    settings        = settings,
    get             = settings$get,
    setMargins      = setMargins,
    initializePng   = initializePng,
    initializeFrame = initializeFrame,
    addPoints       = addPoints,
    addLines        = addLines,
    addBoth         = addBoth,
    addArea         = addArea,
    addLegend       = addLegend,
    addMarginLegend = addMarginLegend,
    finishPng       = finishPng,
    txtFile         = txtFile,
    write.table = function(table, colnames = NULL){
        if(!is.null(colnames)) colnames(table) <- colnames
        write.table(
            table,
            file = txtFile,
            quote = FALSE,
            sep = "\t",
            row.names = FALSE,
            col.names = TRUE
        )
    }
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
