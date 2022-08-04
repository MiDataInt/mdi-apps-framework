#----------------------------------------------------------------------
# static components for a non-interactive, WYSIWYG, publication ready plot
#----------------------------------------------------------------------

# module ui function
staticPlotBoxUI <- function(
    id, 
    width = 6,
    collapsible = TRUE,
    collapsed = FALSE,
    title = NULL
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
            span(
                style = "font-size: 0.8em; margin-left: 10px;", 
                actionLink(ns("reload"), label = icon("sync", verify_fa = FALSE))
            ),
            span(
                style = "font-size: 0.8em; margin-left: 10px;", 
                downloadLink(ns("download"), label = icon("download"))
            ),
            settingsUI(ns('settings'))
        ),
        plotOutput(ns('plot'), inline = TRUE)
    )
}
