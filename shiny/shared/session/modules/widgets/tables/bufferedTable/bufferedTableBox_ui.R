#----------------------------------------------------------------------
# UI components for the bufferedTableBox widget module
#----------------------------------------------------------------------

# module ui function
bufferedTableBoxUI <- function(
    id,
    title,
    ..., # arguments and UI elements passed to shinydashboard::box()
    documentation = FALSE, # same as mdiHeaderLinks()
    reload = FALSE,
    code = FALSE,
    console = FALSE,    
    terminal = FALSE,
    download = FALSE,
    settings = FALSE
) {
    ns <- NS(id)
    mdiBox( # like shinydashboard::box(), expect caller to wrap the box in a fluidRow()
        id = id, 
        title = title,
        documentation = documentation, # same as mdiHeaderLinks()
        reload = reload,
        code = code,
        console = console,    
        terminal = terminal,
        download = download,
        settings = settings,
        ...,
        bufferedTableUI(ns("buffered"))
    )
}
