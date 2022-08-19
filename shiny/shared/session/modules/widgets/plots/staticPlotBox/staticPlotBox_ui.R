#----------------------------------------------------------------------
# static components for a non-interactive, WYSIWYG, publication ready plot
#----------------------------------------------------------------------

# module ui function
staticPlotBoxUI <- function(
    id, 
    width = 6,
    collapsible = TRUE,
    collapsed = FALSE,
    title = NULL,
    documentation = serverEnv$IS_DEVELOPER,
    code = serverEnv$IS_DEVELOPER,
    console = serverEnv$IS_DEVELOPER,
    terminal = FALSE
){

    # initialize namespace
    ns <- NS(id)

    # return the UI contents
    box(
        width = width,
        collapsible = collapsible,
        collapsed = collapsed,
        title = tagList(
            title,
            mdiHeaderLinks(
                id = id,
                type = "box",
                documentation = documentation, 
                reload = TRUE,
                code = code,
                console = console,                    
                terminal = terminal, 
                download = TRUE,
                settings = TRUE
            )
        ),
        plotOutput(ns('plot'), inline = TRUE)
    )
}
