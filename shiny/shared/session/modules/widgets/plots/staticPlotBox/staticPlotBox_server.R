#----------------------------------------------------------------------
# reactive components for a non-interactive, WYSIWYG, publication ready plot
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
staticPlotBoxServer <- function(
    id,
    create = function() NULL, # a function of reactive that creates the plot
    maxHeight = "400px",
    points  = FALSE, # set these to TRUE to expose relevant plot options
    lines   = FALSE,
    legend  = FALSE,
    margins = FALSE,
    title   = FALSE,
    immediate = FALSE # if set to TRUE the plot updates as options are changed
){ moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        module <- 'staticPlotBox' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize module
#----------------------------------------------------------------------
plotId <- ns('plot')
pngFileName <- paste(plotId, "png", sep = ".")
pngFile <- file.path(sessionDirectory, pngFileName)
settingsFile <- file.path(serverEnv$SHARED_DIR, 'session', 'modules', 'widgets', 'plots', 'staticPlotBox', 'settings.yml') # nolint
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
if(!legend) template$Plot_Frame$Legend_Placement <- NULL
if(!title) template$Plot_Frame$Title <- NULL
settings <- settingsServer( # display settings not stored in the UI, exposed by gear icon click
    id = 'settings',
    parentId = id,
    templates = list(template),
    fade = FALSE,
    title = "Plot Parameters",
    immediate = immediate
)

#----------------------------------------------------------------------
# render the plot as a static image filled by caller
#----------------------------------------------------------------------
output$plot <- renderImage({
    ps <- settings$Plot_Frame()
    
    # intialize plot
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
    create()

    # finish plot and return as image
    graphics.off()
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
initializeFrame <- function(...){
    if(margins) setMargins()
    plot(
        NA, 
        NA, 
        typ = "n",
        main = settings$get("Plot_Frame", "Title"),
        ...
    )
}
addPoints <- function(...){
    points(
        pch = if(points) settings$get("Points_and_Lines", "Point_Type") else 19,
        cex = if(points) settings$get("Points_and_Lines", "Point_Size") else 1,
        ...
    )
}
addLines <- function(...){
    lines(
        lty = if(lines) settings$get("Points_and_Lines", "Line_Type")  else 1,        
        lwd = if(lines) settings$get("Points_and_Lines", "Line_Width") else 1,
        ...
    )
}
addLegend <- function(...){
    placement <- if(legend) settings$get('Plot_Frame', 'Legend_Placement') else "topleft"
    if(placement != "none") legend(
        placement,
        lty    = if(lines)  settings$get("Points_and_Lines", "Line_Type")  else NA,        
        lwd    = if(lines)  settings$get("Points_and_Lines", "Line_Width") else NA,
        pch    = if(points) settings$get("Points_and_Lines", "Point_Type") else NA,
        pt.cex = if(points) settings$get("Points_and_Lines", "Point_Size") else NA,
        ...
    )
}

#----------------------------------------------------------------------
# support icon-based file download
#----------------------------------------------------------------------
output$download <- downloadHandler(
    filename = pngFileName,
    content = function(tmpFile) file.copy(pngFile, tmpFile),
    contentType = "image/png"
)

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
    addLegend       = addLegend
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
