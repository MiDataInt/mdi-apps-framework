#----------------------------------------------------------------------
# static components to wrap an interactive plot into a dedicated box
#----------------------------------------------------------------------

# module ui function
interactivePlotBoxUI <- function(
    id, 
    type = c("scatter", "bar", "density"),
    ..., # additional arguments passed to the relevant interactive plot UI function
    #-------------------- for shinydashboard::box()
    title = NULL, 
    footer = NULL, 
    status = NULL,
    solidHeader = FALSE, 
    background = NULL, 
    width = 6, 
    height = NULL,
    collapsible = FALSE, 
    collapsed = FALSE,
    #---------------------------------------- for mdi::box()
    documentation = serverEnv$IS_DEVELOPER, # same as mdiBox()
    reload = FALSE,
    code = serverEnv$IS_DEVELOPER,
    console = serverEnv$IS_DEVELOPER,    
    terminal = FALSE,
    settings = FALSE # download not supported as it is a plotly feature
){
    ns <- NS(id)
    mdiBox(
        id, 
        #--------------------
        title = title, 
        footer = footer, 
        status = status,
        solidHeader = solidHeader, 
        background = background, 
        width = width, 
        height = height,
        collapsible = collapsible, 
        collapsed = collapsed,
        #--------------------
        documentation = documentation, 
        reload = reload,
        code = code,
        console = console,                    
        terminal = terminal, 
        settings = settings,
        switch(
            type,
            scatter = interactiveScatterplotUI(ns("plot"), ...),
            bar = interactiveBarplotUI(ns("plot"), ...),
            density = interactiveDensityPlotUI(ns("plot"), ...)
        )
    ) 
}
