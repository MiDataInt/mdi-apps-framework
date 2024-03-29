#----------------------------------------------------------------------
# reactive components to generate a DT/datatable that uses a buffer
# to minimize repeated redraws of the table
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
bufferedTableServer <- function(
    id,
    parentId,
    parentInput,
    tableData, # reactive, or function with no arguments, that returns the table data
    editBoxes = list(), # e.g., list(editBoxId = list(type=c('checkbox','textbox'), session=session, handler=function(d) d, boxColumn=1, [rawColumn=2])) # nolint
    selection = 'single',
    selectionFn = function(selectedRows) NULL,
    select = NULL, # a reactive that will set the selected row(s)
    options = list(), # passed as is to renderDT
    filterable = FALSE, # add MDI custom filters to all table columns
    server = !filterable, # TRUE for server-side processing; filtering requires client-side, editing requires server-side # nolint
    async = NULL, # for internal use only; set to mdi_async object by asyncTableServer
    settings = NULL, # to add a header icon to the table that will call settings$open()
    rownames = TRUE # some tables may be able to set this to FALSE, but note that doing so can disable proper editing, etc.
) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        parentNS <- NS(parentId)
        module <- 'bufferedTable' # for reportProgress tracing
#----------------------------------------------------------------------

# initialize table proxy and buffer
tableId <- 'table'
selectedId <- paste(tableId, 'rows', 'selected', sep = '_')
proxy <- dataTableProxy(tableId)
buffer <- reactiveVal()
observers <- list()

# render the table
# will redraw anytime tableData invalidates; caller must control via isolate({}) in tableData
output[[tableId]] <- renderDT(
    {
        d <- if(is.null(async)){
            tableData()
        } else {
            x <- async()
            req(x)
            if(x$pending) NULL else x$value
        }
        buffer(d)
        if(filterable) insertColumnFilters(session, tableId, d, rownames = rownames)
        d        
    },
    server = server,
    options = options,
    class = "display table-compact-4",
    escape = FALSE, 
    selection = list(
        mode = selection,
        selected = if(is.null(select)) NULL else select()
    ),
    editable = FALSE,
    rownames = rownames # must be true for editing to work, not sure why (datatables peculiarity)
)

# add the edit box observers that will interact with the buffer
for(editId in names(editBoxes)){
    editBox <- editBoxes[[editId]]
    observers[[editId]] <- observeEvent(parentInput[[editId]], {

        # process the new data
        d <- getTableEditBoxData(parentInput, editId)
        if(editBox$type == 'checkbox'){
            d$newValue <- as.logical(d$newValue)
            newBoxFn <- getTableCheckbox
        } else { # text edit box
            newBoxFn <- getTableEditBox
        }        
        if(!is.null(editBox$handler)) d <- editBox$handler(d) # additional processing by caller; must return d even if not modified

        # update the table proxy, via the buffer, to ensure continued proper display in UI
        buffer <- buffer()
        col <- if(is.null(editBox$boxColumn)) 1 else editBox$boxColumn
        inputId <- if(is.null(editBox$session)) parentNS(editId) else editBox$session$ns(editId)
        buffer[d$selectedRow, col] <- newBoxFn(
            inputId, # required column that carries the edit box for updating the value
            d$selectedRow,
            d$newValue
        )
        if(!is.null(editBox$rawColumn)) { # optional column that carries the raw, uneditable value
            buffer[d$selectedRow, editBox$rawColumn] <- d$newValue
        }
        buffer(buffer)
    })    
}

# observe row selection and pass back to caller
if(selection != 'none' && !is.null(selectionFn)){
    observers$rowSelection <- observeEvent(input[[selectedId]], {
        selectionFn(input[[selectedId]])
    })    
}

# update the row selection based on a caller's reactive
if(!is.null(select)) observers$selectRows <- observeEvent(select(), {
    selectRows(proxy, select())
})

# update the table data without a complete redraw, i.e. updates "in place"
# executed whenever the buffer changes
observers$bufferChange <- observeEvent(buffer(), {
    req(server) # replaceData() is inconsistent with client-side processing
    buffer <- buffer()
    req(buffer)
    isolate({ replaceData(proxy, buffer, resetPaging = FALSE, clearSelection = "none") })
})

# function to help the caller update a single cell in the table
updateCell <- function(row, col, value, rowCol=NULL){
    if(is.null(row)) return(NULL) # not req(), don't crash the caller if can't update
    if(is.null(col)) return(NULL)
    buffer <- buffer()
    if(is.null(buffer)) return(NULL)
    if(is.character(row)) row <- which(buffer[[rowCol]] == row)
    buffer[row, col] <- value
    buffer(buffer)
}

#----------------------------------------------------------------------
# support icon-based file download
#----------------------------------------------------------------------
csvId <- ns('data')
csvFileName <- paste(csvId, "csv", sep = ".")
output$download <- downloadHandler(
    filename = csvFileName,
    content = function(tmpFile) {
        x <- tableData()
        if(nrow(x) > 0){
            cols <- names(x)
            inputs <- which(apply(x[1], 1, function(v){
                v <- as.character(v)
                grepl("<input", v)
            }))
            if(length(inputs) > 0) x <- x[, .SD, .SDcols = cols[-inputs]]
        }
        write.csv(
            x, 
            tmpFile,
            row.names = FALSE
        )
    },
    contentType = "text/csv"
)

#----------------------------------------------------------------------
# support opening a settings modal, typically one that controls the table's contents
#----------------------------------------------------------------------
if(is.list(settings)) observers$openSettings <- observeEvent(input$openSettings, {
    settings$open()
})

#----------------------------------------------------------------------
# set return values
#----------------------------------------------------------------------
list(
    rows_selected = reactive({ input[[selectedId]] }), # alternative way for caller to use selected rows
    selectionObserver = rowSelectionObserver(tableId, input),
    selectRows = function(rows) selectRows(proxy, rows), # for setting the row selection
    updateCell = updateCell,
    buffer = buffer,
    observers = observers
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
