#----------------------------------------------------------------------
# reactive components to asynchronously fill an HTML div element
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
asyncDivServer <- function(
    id,
    dataFn, # a function to fill a reactiveVal with the data needed to render the div
    uiFn,   # a function to render the div's UI using the data returned by dataFn
    async = TRUE, # in select circumstances, caller may wish to force synchronous execution of the same widget
    maskIds = character(), # addition ids to mask when async = FALSE, used "asis", caller must apply ns()
    ...     # additional arguments passed to tags$div
) {
    moduleServer(id, function(input, output, session) {
        module <- 'asyncDiv' # for reportProgress tracing
#----------------------------------------------------------------------

# asynchronously collect the data that supports the div
name <- paste0("asyncDiv:", id)
data_async <- reactiveVal(NULL)
data <- reactive({
    async <- data_async()
    req(async)
    req(!async$pending)
    async$value
})

# render the UI
maskDiv <- function(masked){ # for synchronous feedback
    if(masked) startSpinner(session, name) else stopSpinner(session, name)
    session$sendCustomMessage('maskElement', list(id = session$ns("div"), masked = masked))
    for(id in maskIds) session$sendCustomMessage('maskElement', list(id = id, masked = masked))
}
if(async) output$progress <- renderUI({
    async <- data_async()
    req(async)
    req(async$pending)
    mdiIndeterminateProgressBar()
}) else observeEvent(data_async(), {
    async <- data_async()
    req(async)
    if(!async$pending) maskDiv(FALSE)
})
output$div <- renderUI({ 
    tags$div(uiFn(data()), ...)
})

#----------------------------------------------------------------------
# set return value
#----------------------------------------------------------------------
list(
    data = data, # return the data reactive for potentially other uses in the calling widget
    update = function(...){ # method to force asynchronous update of the div
        if(!async) maskDiv(TRUE)
        mdi_async(dataFn, data_async, name, async = async, ...) # ... arguments passed to dataFn
    }
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
