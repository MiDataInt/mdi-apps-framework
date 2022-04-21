#----------------------------------------------------------------------
# reactive components for a tabular view of analysis results, with download link
# typically in a viewResults module
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
resultsTableServer <- function(
    id,
    parentId,
    type=c('short', 'long'),
    tableData, # reactive (or function with no arguments) that returns the data table to display # nolint
    getFileName, # function with no argument that returns a file name (not path)
    marks = NULL, # a reactiveVal that is an updatable logical outcomes vector whether to mark (not select) rows # nolint
    parentNS = NULL # required if marks is not null
) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        module <- ns('resultsTable') # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize variables
#----------------------------------------------------------------------

# named control for specific table presentations
dtOptions <- list(
    short = list(
        paging = FALSE,
        searching = FALSE  
    ),
    long = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 100)
    )
)

#----------------------------------------------------------------------
# render the table
#----------------------------------------------------------------------
tableId <- 'table'
markBoxId <- 'rowMarked'
proxy <- dataTableProxy(tableId)
buffer <- reactiveVal()
getMarkBoxId <- function() parentNS(ns(markBoxId))
output$table <- renderDT(
    {
        dt <- tableData()
        req(dt)
        dt <- data.table(dt)
        dt <- if(!is.null(marks)){
            dt[, ':='(
                MARK = tableCheckboxes(getMarkBoxId(), isolate({ marks() }) ),
                Marked = isolate({ marks() })
            )]
            markCols <- c('MARK', 'Marked')
            dt[, .SD, .SDcols = c(markCols, names(dt)[names(dt) %notin% markCols]) ]
        } else dt
        buffer(dt)
        dt
    },
    options = dtOptions[[type]], 
    class = "display table-compact-2",
    escape = FALSE,
    selection = 'single',
    editable = FALSE,
    filter = if(type == "long") "top" else "none"
)

#----------------------------------------------------------------------
# if marked, handle updates of marking status
#----------------------------------------------------------------------
if(!is.null(marks)) observeEvent(input[[markBoxId]], {

    # process the new data
    d <- getTableEditBoxData(input, markBoxId)
    d$newValue <- as.logical(d$newValue)
    x <- marks()
    x[d$selectedRow] <- d$newValue
    marks(x) 

    # update the table proxy, via the buffer, to ensure continued proper display in UI
    buffer <- buffer()
    buffer[d$selectedRow, 1] <- getTableCheckbox(getMarkBoxId(), d$selectedRow, d$newValue)
    buffer[d$selectedRow, 2] <- d$newValue
    buffer(buffer)
})    
observeEvent(buffer(), {
    buffer <- buffer()
    req(buffer)
    isolate({ replaceData(proxy, buffer, resetPaging = FALSE, clearSelection = "none") })
})

#----------------------------------------------------------------------
# allow user to download the table
#----------------------------------------------------------------------
output$download <- downloadHandler(
    filename = function() {
        paste(gsub(' ', '_', getFileName()), "csv", sep = ".")
    },
    content = function(file) {
        write.csv(tableData(), file, row.names = FALSE)
    }
)

#----------------------------------------------------------------------
# return reactiveValues
#----------------------------------------------------------------------
list(
    selected = rowSelectionObserver(tableId, input),
    selectRow = function(i) selectRows(proxy, i)
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
