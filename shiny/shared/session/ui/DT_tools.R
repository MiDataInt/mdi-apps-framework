#----------------------------------------------------------------------
# shared/ui/DT_tools.R provides widgets to maximize utility of data tables
#----------------------------------------------------------------------
# the MDI uses the R DT package for table rendering
# DT is a general R package, not specific to Shiny, but renderDT function wraps DT to Shiny
# DT manipulates the JavaScript library called DataTables (which it loads for us)
# see:
#   https://rstudio.github.io/DT/ <<-- R package, options passed as renderDT arguments
#   https://cran.r-project.org/web/packages/DT/DT.pdf <<-- best documentation here
#   https://rstudio.github.io/DT/shiny.html
#   https://datatables.net/manual/options  <<-- JavaScript library, options set as renderDT(options=list())
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# helper functions for constructing unique widget ids that can be properly parsed
#----------------------------------------------------------------------

# separator we'll use that shouldn't be found in any incoming ids
tableUiInputSeparator <- '___'

# a simple counter to ensure that every load of every table has a unique id
#   without this, reloads of the same table have same ids and they go stale and don't work
#   this value is never used downstream, it simply ensures uniqueness for Shiny event firing
tableUILoadCounter <- 1
setUILoadCounter <- function(){ # every function creating a column of inputs calls this function 
    tableUILoadCounter <<- tableUILoadCounter + 1
}

# instanceIds assign a unique id to each row's element, with a common parent prefix
getInstanceId <- function(parentId, i, j=0){ # i = row number, j = optional item number in row
    paste(parentId, i, j, tableUILoadCounter, sep=tableUiInputSeparator)
}

# assemble a vector of UI inputs suitable for embedding as a data table column
# request is for a set of inputs, one per row
tableUiInputs <- function(ShinyUiFun, parentId, nrow, ..., allow=NULL) { # ShinyUiFun = actionButton, etc.
    setUILoadCounter()
    inputs <- character(nrow)
    if(is.null(allow)) allow <- rep(TRUE, nrow)
    for (i in seq_len(nrow)) {
        instanceId <- getInstanceId(parentId, i)    
        ui <- if(allow[i]) ShinyUiFun(instanceId, ...) else ""
        inputs[i] <- as.character(tags$div(onmousedown="event.stopPropagation();", ui))
    }
    inputs 
}
# request is for a single input in a single numbered row
tableUiInput <- function(ShinyUiFun, parentId, rowN, ..., allow=NULL, j=0) { # ShinyUiFun = actionButton, etc.
    setUILoadCounter()  
    if(is.null(allow)) allow <- TRUE
    instanceId <- getInstanceId(parentId, rowN, j) 
    ui <- if(allow) ShinyUiFun(instanceId, ...) else ""
    as.character(tags$div(onmousedown="event.stopPropagation();", ui))
}

#----------------------------------------------------------------------
# dedicated action links in all rows in a single column
#----------------------------------------------------------------------
  
# put one identical action link in every table row ...
# javascript triggers a _single_ event for all links, which carries the row information
tableActionLinks <- function(parentId, nrow, label, ..., confirmMessage=NULL, allow=NULL) {
    if(is.null(confirmMessage)) confirmMessage <- "NO_CONFIRM"
    onclick <- getTableActionOnClick(parentId, confirmMessage)
    tableUiInputs(actionLink, parentId, nrow, label, ..., onclick=onclick, allow=allow)
}
getTableActionOnClick <- function(parentId, confirmMessage){
    paste0('handleActionClick("', parentId, '", this.id, "', confirmMessage, '")')
}
# ... and recover the row index when the action is clicked
getTableActionLinkRow <- function(input, parentId){
    as.numeric(strsplit(input[[parentId]], tableUiInputSeparator)[[1]][2])
}

# or get potentially multiple action links to fill just one cell
tableCellActionLinks <- function(parentId, rowN, labels, ..., confirmMessage=NULL, allow=NULL) {
    if(is.null(labels) || length(labels) == 0) return("NA")    
    if(is.null(confirmMessage)) confirmMessage <- "NO_CONFIRM"
    onclick <- getTableActionOnClick(parentId, confirmMessage)
    HTML(paste(unname(sapply(1:length(labels), function(labelN){
        tableUiInput(actionLink, parentId, rowN, labels[labelN], ...,
                      onclick=onclick, allow=allow, j=labelN)
    })), collapse=''))
}
# ... and recover the row and item indices when the action is clicked
getTableActionLinkRowAndItem <- function(input, parentId){
    as.integer(strsplit(input[[parentId]], tableUiInputSeparator)[[1]][2:3])
}

#----------------------------------------------------------------------
# special action links to prevent inappropriate record removal based on data locks
#----------------------------------------------------------------------
tableRemoveActionLinks <- function(stepLocks, parentId, confirmMessage, ids, ...) {
    label  <- "Remove"
    nrow   <- length(ids)
    locked <- sapply(ids, function(id) !is.null(stepLocks[[id]]) &&
                                       length(stepLocks[[id]]) > 0)
    tableActionLinks(parentId, nrow, label, ...,
                     confirmMessage=confirmMessage, allow = !locked)
}

#----------------------------------------------------------------------
# special action links to control execution of analysis jobs
#   provides a link to launch jobs, or feedback on job status
#----------------------------------------------------------------------
tableJobActionLinks <- function(parentId, statuses, ...) {
    setUILoadCounter()
    linkName <- "Launch Job" # TODO: make a nicer launcher button    
    confirmMessage <- "Launch the analysis job?" # is this needed, or just let it happen?
    onclick <- getTableActionOnClick(parentId, confirmMessage)
    nrow <- length(statuses)
    inputs <- character(nrow)
    for (i in seq_len(nrow)) {
        inputs[i] <- if(is.na(statuses[i])) ""
        else if(statuses[i] == CONSTANTS$jobStatuses$created$value) { # i.e. a pending job
            instanceId <- getInstanceId(parentId, i)
            as.character(tags$div(onmousedown="event.stopPropagation();",
                                  actionLink(instanceId, linkName, onclick=onclick, ...)))
        } else CONSTANTS$jobStatuses[[statuses[i] + 3]]$icon
    }
    inputs
}

#----------------------------------------------------------------------
# dedicated cell edit boxes in all rows in a single column
#   leaves an edit box permanently on display in desired columns only
#   an alternative approach to using editable = TRUE or other in renderDT
#----------------------------------------------------------------------

# put an identical edit box in every table row ...
tableEditBoxes <- function(parentId, defaults, createFn=NULL, field='value', ...){
    setUILoadCounter()
    onchange <- getTableEditBoxOnChange(parentId, field)    
    nrow <- length(defaults)
    inputs <- character(nrow)
    if(is.null(createFn)) createFn <- getTableEditBox
    for (i in seq_len(nrow)) {
        inputs[i] <- createFn(parentId, i, defaults[i], onchange)    
    }
    inputs
}
getTableEditBox <- function(parentId, i, default, onchange=NULL){ # need this to replace 1 box when editing
    if(is.null(onchange)){
        setUILoadCounter()
        onchange <- getTableEditBoxOnChange(parentId)
    }
    instanceId <- getInstanceId(parentId, i)
    paste0( # forced to do the hard way since textInput doesn't override onchange
        '<input class="DT-edit-box" id="', instanceId,
        '" type="text", onmousedown="event.stopPropagation();" onchange="', onchange,
        '" value="', default,
        '"/>'
    )    
}
getTableEditBoxOnChange <- function(parentId, field="value"){
    paste0('Shiny.setInputValue(\'', parentId, '\',  this.id + \'',
            tableUiInputSeparator, '\' + this.', field, ', {priority: \'event\'})')
}
 
# ... and recover the incoming edited value and associated table row
getTableEditBoxData <- function(input, parentId){
    parts <- strsplit(input[[parentId]], tableUiInputSeparator)[[1]]
    list(
        selectedRow = as.numeric(parts[2]),
        newValue = parts[5]
    )
}

#----------------------------------------------------------------------
# add a checkbox column to allow user to set/unset a flag per row
#----------------------------------------------------------------------

# put an identical checkbox in every table row ...
tableCheckboxes <- function(parentId, defaults, ...){
    tableEditBoxes(parentId, defaults, createFn=getTableCheckbox, field='checked', ...)
}
getTableCheckbox <- function(parentId, i, default, onchange=NULL){ # need this to replace 1 box when editing
    if(is.null(onchange)){
        setUILoadCounter()
        onchange <- getTableEditBoxOnChange(parentId, 'checked')
    }
    instanceId <- getInstanceId(parentId, i)
    value <- 'checked'
    checked <- if((is.logical(default) && default) || default == value) 'checked' else ''
    paste0( # forced to do the hard way since Shiny input doesn't override onchange
        '<input class="DT-edit-box DT-checkbox" id="', instanceId,
        '" type="checkbox", onmousedown="event.stopPropagation();" onchange="', onchange,
        '" value="', value, '" name="', instanceId, '" ', checked, '/>'
    )    
}

#----------------------------------------------------------------------
# control the reactive selection of parent table rows more carefully than Shiny
#   prevents cascading tables from invalidating twice on 1st load of parent table
#   basically, convert any state other than a valid row selection to NA (not NULL!)
#----------------------------------------------------------------------
rowSelectionObserver <- function(parentTable, input){
    selected <- reactiveVal(NA)
    inputId <- paste(parentTable, "rows_selected", sep="_")
    observe({
        row <- input[[inputId]]
        if(is.null(row)) row <- NA
        selected(row)
    })
    selected
}
