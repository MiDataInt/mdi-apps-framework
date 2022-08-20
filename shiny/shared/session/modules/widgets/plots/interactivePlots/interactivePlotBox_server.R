#----------------------------------------------------------------------
# reactive components to wrap an interactive plot into a dedicated box
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
interactivePlotBoxServer <- function(
    id, 
    type = c("scatter", "bar", "density"),
    ..., # additional arguments passed to the relevant interactive plot server function
    #---------------------------- passed to activateMdiHeaderLinks()
    url = getDocumentationUrl(
        "shiny/shared/session/modules/widgets/plots/interactivePlots/README", 
        framework = TRUE
    ),
    reload = NULL, # callback function with no arguments to handle the reload action
    baseDirs = NULL, # in addition to plots/interactivePlots
    envir = parent.frame(),
    dir = NULL,
    settings = NULL, # an additional settings template as a list()
    template = settings, # for legacy support
    #---------------------------- passed to settingsServer()
    cacheKey = NULL, # a reactive/reactiveVal that returns an id for the current settings state
    immediate = FALSE, # if TRUE, setting changes are transmitted in real time
    resettable = TRUE  # if TRUE, a Reset All Setting link will be provided
){ moduleServer(id, function(input, output, session) {   
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize module and the plot we are wrapping
#----------------------------------------------------------------------
module <- 'interactivePlotBox' # for reportProgress tracing
switch(
    type,
    scatter = interactiveScatterplotServer("plot", ...),
    bar = interactiveBarplotServer("plot", ...),
    density = interactiveDensityPlotServer("plot", ...)
)

#----------------------------------------------------------------------
# return any settings from the activated box
#----------------------------------------------------------------------
settings <- activateMdiHeaderLinks(
    session,
    #----------------------------
    url = url, # documentation
    reload = reload, # reload
    baseDirs = unique(c( # code editor
        baseDirs,
        getWidgetDir("plots/interactivePlots", framework = TRUE)
    )),
    envir = envir, # R console
    dir = dir, # terminal emulator
    settings = module, # plot settings
    templates = list(template),
    fade = FALSE,
    title = "Plot Parameters",
    cacheKey = cacheKey,
    immediate = immediate,
    resettable = resettable
)
settings

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
