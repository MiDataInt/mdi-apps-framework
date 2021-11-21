#----------------------------------------------------------------------
# static components to an interactive density plot using plot_ly
#----------------------------------------------------------------------

# module ui function
interactiveDensityPlotUI <- function(id) {

    # initialize namespace
    ns <- NS(id)

    # return the UI contents
    plotlyOutput(ns('plotly'))
}
