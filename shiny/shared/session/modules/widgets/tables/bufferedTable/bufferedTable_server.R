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
    editBoxes = list(), # e.g., list(editBoxId = list(type=c('checkbox','textbox'), handler=function(d), boxColumn=1, [rawColumn=2])) # nolint
    selection = 'single',
    selectionFn = function(selectedRows) NULL,
    options = list() # passed as is to renderDT
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

# render the table
# will redraw anytime tableData invalidates; caller must control via isolate({}) in tableData
output[[tableId]] <- renderDT(
    {
        d <- tableData()
        buffer(d)
        d
    },
    options = options,
    class = "display table-compact-4",
    escape = FALSE, 
    selection = selection, 
    editable = FALSE,
    rownames = TRUE # must be true for editing to work, not sure why (datatables peculiarity)
)

# add the edit box observers that will interact with the buffer
for(editId in names(editBoxes)){
    editBox <- editBoxes[[editId]]
    observeEvent(parentInput[[editId]], {

        # process the new data
        d <- getTableEditBoxData(parentInput, editId)
        if(editBox$type == 'checkbox'){
            d$newValue <- as.logical(d$newValue)
            newBoxFn <- getTableCheckbox
        } else { # text edit box
            newBoxFn <- getTableEditBox
        }        
        d <- editBox$handler(d) # additional processing by caller; must return d even if not modified

        # update the table proxy, via the buffer, to ensure continued proper display in UI
        buffer <- buffer()
        buffer[d$selectedRow, editBox$boxColumn] <- newBoxFn(
            parentNS(editId), # required column that carries the edit box for updating the value
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
    observeEvent(input[[selectedId]], {
        selectionFn(input[[selectedId]])
    })    
}

# update the table data without a complete redraw, i.e. updates "in place"
# executed whenever the buffer changes
observeEvent(buffer(), {
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
    content = function(tmpFile) write.csv(
        tableData(),
        tmpFile
    ),
    contentType = "text/csv"
)

#----------------------------------------------------------------------
# set return values
#----------------------------------------------------------------------
list(
    rows_selected = reactive({ input[[selectedId]] }), # alternative way for caller to use selected rows
    selectRows = function(rows) selectRows(proxy, rows), # for setting the row selection
    updateCell = updateCell,
    buffer = buffer
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
