#----------------------------------------------------------------------
# reactive components for selecting one or more data sources in an analysis set
# most useful for apps that do not have samples
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
dataSourceTableServer <- function(
    id, 
    selection = "single"
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
        sourcesSummary()[, c('FileName', 'Project')]
    },
    options = list(
        paging = FALSE,
        searching = FALSE  
    ),
    # class = "display table-compact-4",
    escape = TRUE,
    selection = selection,
    editable = FALSE, 
    rownames = FALSE # must be true for editing to work, not sure why (datatables peculiarity)
)

#----------------------------------------------------------------------
# return a reactive populated with the id(s) of the selected source(s)
#----------------------------------------------------------------------
selectedSourceIds

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
