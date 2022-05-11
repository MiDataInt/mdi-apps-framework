#----------------------------------------------------------------------
# reactive components to an interactive density plot using plot_ly
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
interactiveDensityPlotServer <- function(id, reactive) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        module <- 'interactiveDensityPlot' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize module
#----------------------------------------------------------------------
plotId <- ns('plot')
selectionEvent <- quote(event_data("plotly_selected", source = plotId))
selected <- reactiveVal()

#----------------------------------------------------------------------
# render the plot
#----------------------------------------------------------------------
output$plotly <- renderPlotly({
    d <- reactive()
    req(d)
    d <- density(na.omit(d))
    plot_ly(
        data.frame(x = d$x, y = d$y), # plot_ly demands a data.frame, not a simple list
        x = ~x,
        y = ~y,
        type = 'scatter',
        mode = 'markers',
        source = plotId
    )
})

#----------------------------------------------------------------------
# respond to an interactive selection event of data points by fitting a curve
#----------------------------------------------------------------------
observeEvent(selectionEvent, {
    d <- eval(selectionEvent)
    selected(d)
    print(head(d))
}, event.quoted = TRUE)

#----------------------------------------------------------------------
# set return values as reactives that will be assigned to app$data[[stepName]]
#----------------------------------------------------------------------
list(
    selected = selected
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
