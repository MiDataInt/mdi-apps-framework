#----------------------------------------------------------------------
# reactive components to asynchronously fill a bufferedTable
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
asyncTableServer <- function(
    id,
    createTableFn, # a function that creates the table data frame; must not call reactives
    ...            # additional arguments passed to bufferedTableServer
) {
    moduleServer(id, function(input, output, session) {
        module <- 'asyncTable' # for reportProgress tracing
#----------------------------------------------------------------------

# asynchronously collect the table data
name <- paste0("asyncTable:", id)
tableData_async <- reactiveVal(NULL)
tableData <- reactive({
    async <- tableData_async()
    req(async)
    req(!async$pending)
    async$value
})

# render the UI
output$progress <- renderUI({
    async <- tableData_async()
    req(async)
    req(async$pending)
    mdiIndeterminateProgressBar()
})
table <- bufferedTableServer(
    "table", 
    parentId = id, 
    parentInput = input, 
    tableData = tableData, 
    async = tableData_async, 
    ...
)

#----------------------------------------------------------------------
# set return value
#----------------------------------------------------------------------
list(
    tableData = tableData, # reactive with the tabulated data
    table = table, # the buffered table object, but rows_selected, etc.
    update = function(...){ # method to force asynchronous update of the table
        mdi_async(createTableFn, tableData_async, name, ...) # ... arguments passed to createTableFn
    }
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
