#----------------------------------------------------------------------
# static components for a box wrapper around mdiInteractivePlot
#----------------------------------------------------------------------

# module ui function
mdiInteractivePlotBoxUI <- function(
    id, 
    title,
    ..., # additional arguments passed to shinydashboard::box()
    documentation = serverEnv$IS_DEVELOPER,
    code = serverEnv$IS_DEVELOPER,
    console = serverEnv$IS_DEVELOPER,
    terminal = FALSE,
    data = FALSE
){
    ns <- NS(id)
    mdiBox(
        id, 
        title,
        documentation = documentation, 
        reload = TRUE,
        code = code,
        console = console,
        terminal = terminal, 
        download = TRUE,
        data = data,
        settings = TRUE,
        ...,
        mdiInteractivePlotUI(ns('plot'))
    )     
}
