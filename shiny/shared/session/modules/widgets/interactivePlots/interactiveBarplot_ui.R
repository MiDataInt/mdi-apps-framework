#----------------------------------------------------------------------
# static components to an interactive horizontal or vertical barplot using plot_ly
#----------------------------------------------------------------------

# module ui function
interactiveBarplotUI <- function(id, height='300px') {

    # initialize namespace
    ns <- NS(id)

    # return the UI contents
    plotlyOutput(ns('plotly'), height = height)
}
