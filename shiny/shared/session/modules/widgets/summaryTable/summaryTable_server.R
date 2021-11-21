#----------------------------------------------------------------------
# reactive components for a tabular view of data, with standardized special columns
# in particular, these are the tabular summaries at the bottom of appStep modules
#----------------------------------------------------------------------
# returns:
#   data reativeValues with list, names, ids, summary and selected row
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
summaryTableServer <- function(
    id, parentId, stepNumber, stepLocks, sendFeedback,
    template, type,
    remove=NULL, names=NULL, parent=NULL,
    clearLocks=NULL, statusChange=NULL
) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        parentNS <- function(id) paste(parentId, ns(id), sep = "-")
        module <- ns('table') # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize variables
#----------------------------------------------------------------------

# request options
isRemove   <- !is.null(remove)
isNameEdit <- !is.null(names)
isJobLauncher <- 'Job_Status' %in% names(template)

# the returned reactiveValues
data <- reactiveValues(
    list     = list(),
    ids      = list(),    
    names    = list(),
    summary  = template,
    proxy    = dataTableProxy('table'),
    selected = rowSelectionObserver('table', input),
    clearLocks = clearLocks,
    purgeOutput = isJobLauncher
)

# named control for specific table presentations
dtOptions <- list(
    shortList = list(
        paging = FALSE,
        searching = FALSE  
    ),
    longList100 = list(
        lengthMenu = c(20, 100, 500),
        pageLength = 100
    )
)

# table control objects
removeRowId <- 'removeRow'
editNameId <- 'editName'
launchJobId <- 'launchJob'
buffer <- NULL # used for updating proxy in table with edit boxes
buffer_ <- reactiveVal()

#----------------------------------------------------------------------
# render the summary table
#----------------------------------------------------------------------
output$table <- renderDT(
    {
        for(x in names(stepLocks)) stepLocks[[x]] # ensure reactivity to current lock values
        getSummaryTableData(module, data$summary, buffer_, parent, modifySummary)
    },
    options = dtOptions[[type]],
    class = "display table-compact-4",
    escape = FALSE, 
    selection = 'single', 
    editable = FALSE, 
    rownames = TRUE # must be true for editing to work, not sure why (datatables peculiarity)
)

#----------------------------------------------------------------------
# fill in special record widgets during render
#----------------------------------------------------------------------
modifySummary <- function(summary, parentId){

    # apply filtering of this table to match the id of a selected row in a parent table
    rows <- if(is.null(parentId)){
        TRUE
    } else {
        which( data$list[[parent$keyColumn]] == parentId )
    }
    summary <- summary[rows, ]
    nrow <- nrow(summary)
    
    # add custom record removal boxes
    if(isRemove){
        summary$Remove <- tableRemoveActionLinks(
            stepLocks = stepLocks,
            parentId = parentNS(removeRowId),
            confirmMessage = NULL,
            ids = data$ids[rows]
        )
    }

    # add custom name editing boxes
    if(isNameEdit){
        summary$Name <- tableEditBoxes(
            parentId = parentNS(editNameId),
            #defaults = names$get(names$source, rows)
            defaults = names$get(rows)
        )
    }

    # add custom job launch options
    if(isJobLauncher){
        summary$Job_Status <- tableJobActionLinks(
            parentId = parentNS(launchJobId),
            statuses = summary$Job_Status
        )        
    }

    # return the modified summary table
    summary
}

#----------------------------------------------------------------------
# activate record removal action
#----------------------------------------------------------------------
if(isRemove){
    addRemoveObserver(input, removeRowId, module, data, sendFeedback, remove)   
}

#----------------------------------------------------------------------
# activate record name edit action
#----------------------------------------------------------------------
if(isNameEdit){
    colI <- which(names(template) == 'Name')
    addNameEditObserver(input, editNameId, module, data, buffer_, parentNS, colI)
    observe({
        buffer <- buffer_()
        req(buffer)
        isolate({ replaceData(data$proxy, buffer, resetPaging = FALSE, clearSelection = "none") })
        #sendFeedback(NULL)
    })    
}

#----------------------------------------------------------------------
# activate job launch action
#----------------------------------------------------------------------
if(isJobLauncher){
    statusChange <- reactiveVal(NULL)
    addJobLaunchObserver(session, input, module,
                         data, launchJobId,
                         sendFeedback,
                         statusChange)
    addStatusChangeObserver(module, data, statusChange)
}

#----------------------------------------------------------------------
# update flow control based on table status
#----------------------------------------------------------------------
observe({
    name <- parentNS('count')
    count <- length(data$ids)
    
    # set javascript variable used to trigger conditional panels
    session$sendCustomMessage('updateTrigger', list(name = name, value = count))
})

#----------------------------------------------------------------------
# return reactiveValues
#----------------------------------------------------------------------
data

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
