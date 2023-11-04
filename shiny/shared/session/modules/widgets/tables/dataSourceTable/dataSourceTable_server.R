#----------------------------------------------------------------------
# reactive components for selecting one or more data sources in an analysis set
# most useful for apps that do not have samples
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
dataSourceTableServer <- function(
    id, 
    selection = "single",
    escape = TRUE,
    extraColumns = NULL, # function or reactive that returns extra display columns as a data.table
    sourceIdOverride = NULL # a reactive that allows calling module to programatically set the selected row
) {
    moduleServer(id, function(input, output, session) {
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# collect the list of data sources that have been uploaded into the analysis set
#----------------------------------------------------------------------
sourcesSummary <- getStepReturnValueByType('upload', 'sourcesSummary')   # for display
sources        <- getStepReturnValueByType('upload', 'outcomes')$sources # for selection return

#----------------------------------------------------------------------
# track the table, i.e., data source selection
#----------------------------------------------------------------------
selectedRows <- rowSelectionObserver('table', input)
if(!is.null(sourceIdOverride)) observeEvent(sourceIdOverride(), {
    sourceIds <- sourceIdOverride()
    if(isTruthy(sourceIds)){
        rows <- which(names(sources()) %in% sourceIds)
        selectedRows(rows)
        selectRows(tableProxy, rows)
    } else {
        selectedRows(NA)
        selectRows(tableProxy, NA)
    }
})
selectedSourceIds <- reactive({
    rows <- selectedRows()
    req(rows)
    names(sources()[rows])
})

#----------------------------------------------------------------------
# render the selection table
#----------------------------------------------------------------------
output$table <- renderDT(
    {
        req(sourcesSummary)
        dt <- sourcesSummary()[, c('FileName', 'Project')]
        if(is.null(extraColumns)) dt else cbind(dt, extraColumns()) 
    },
    options = list(
        paging = FALSE,
        searching = FALSE  
    ),
    class = "display table-compact-4",
    escape = escape,
    selection = selection,
    editable = FALSE, 
    rownames = FALSE # must be true for editing to work, not sure why (datatables peculiarity)
)
tableProxy <- dataTableProxy("table")

#----------------------------------------------------------------------
# return a reactive populated with the id(s) of the selected source(s)
#----------------------------------------------------------------------
selectedSourceIds

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
