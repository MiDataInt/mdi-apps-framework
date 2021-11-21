#----------------------------------------------------------------------
# static components to an interactive XY scatter plot using plot_ly
#----------------------------------------------------------------------

# module ui function
interactiveScatterplotUI <- function(id, height='300px') {

    # initialize namespace
    ns <- NS(id)

    # return the UI contents
    plotlyOutput(ns('plotly'), height = height)
}
