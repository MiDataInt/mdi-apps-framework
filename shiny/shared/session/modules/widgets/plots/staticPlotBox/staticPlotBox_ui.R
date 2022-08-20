#----------------------------------------------------------------------
# static components for a non-interactive, WYSIWYG, publication ready plot
#----------------------------------------------------------------------

# module ui function
staticPlotBoxUI <- function(
    id, 
    title,
    ..., # additional arguments passed to shinydashboard::box()    
    documentation = serverEnv$IS_DEVELOPER,
    code = serverEnv$IS_DEVELOPER,
    console = serverEnv$IS_DEVELOPER,
    terminal = FALSE
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
        settings = TRUE,
        ...,
        plotOutput(ns('plot'), inline = TRUE)
    )     
}
