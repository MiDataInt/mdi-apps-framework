#----------------------------------------------------------------------
# many modules follow the archetype of a summary list + item edit form
# functions here support coordination between the two and construction of modules
# representative example modules are assignSamples and runAnalyses
#----------------------------------------------------------------------
# record sets are lists that carry hashes as UIDs in the list names
# these hashed ids are used to name files stored on disk
#----------------------------------------------------------------------
# records can additionally have human-readable names stored outside of the record itself
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# help handle record save/edit actions (from the edit form)
#   these are generally called by the appStep module itself
#----------------------------------------------------------------------

# determine the state of the editing form relative to existing records
initializeRecordEdit <- function(d, workingId, list, nameName, errorName, sendFeedback){
    
    # set the hashed record id
    # it is critical that 'd' has ALL OF but ONLY the values that uniquely define a record
    id <- digest(d, "md5", serialize = TRUE)
    
    # determine how the pending record relates to existing records
    isKnown <- !is.null(list[[id]])    
    isEdit  <- !is.null(workingId)
    isSame  <- if(isEdit) id == workingId else FALSE
    if((isKnown &  isEdit & !isSame) |
       (isKnown & !isEdit)) sendFeedback(paste(errorName, 'already exists'), TRUE)
    
    # return our results
    # caller may continue to add non-defining 'd' values
    list(
        id = id,
        isKnown = isKnown,
        isEdit = isEdit,
        isSame = isSame,
        name = if(isEdit) list[[workingId]]$name else paste0(nameName, ' #', length(list) + 1)    
    )
}

# commit a new/edited record
saveEditedRecord <- function(d, workingId, data, r){
    
    # caller has now completed the values in 'd'
    data$list[[r$id]]  <- d
    data$names[[r$id]] <- if(is.null(data$names[[r$id]])) d$name else data$names[[r$id]]
    
    # if definitional record contents have changed, remove the record with the old key
    if(r$isEdit & !r$isKnown){
        data$list[[workingId]]  <- NULL 
        data$names[[workingId]] <- NULL
    }  
}

#----------------------------------------------------------------------
# help construct a summary table display of all records
#----------------------------------------------------------------------

# create data$summary in reaction to data$list changes
#   generally called by the appStep module itself
addDataListObserver <- function(module, template, data, dataFrame){
    observe({
        reportProgress('observe data$list', module)
        table <- template
        for(id in names(data$list)){
            table <- rbind(table, dataFrame(data$list[[id]], id))
        }
        data$summary <- table
        data$ids <- names(data$list)
    })
}

# help construct the summary table data as passed to renderDT
#   generally called by the summaryTable module (not not necessarily)
getSummaryTableData <- function(module, summary, buffer, parent, modify){
    reportProgress('output$summaryTable', module)
    nrow <- nrow(summary)
    parentId <- if(is.null(parent)){
        NULL
    } else {
        parentRow <- parent$table$selected()
        if(is.na(parentRow)) NULL else parent$table$ids[parentRow]
    }
    isolate({
        table <- if(nrow > 0) modify(summary, parentId) else NULL # caller can modify table for display
        if(!is.null(buffer)) buffer(table)
        table
    })
}

#----------------------------------------------------------------------
# help handle record remove and edit actions (from the summary table)
#   these are generally called by the summaryTable module (not not necessarily)
#----------------------------------------------------------------------

# remove record action
# delete option also allows for deletion of an associated server source file
addRemoveObserver <- function(input, inputId, module, data, sendFeedback = NULL, 
                              remove = NULL, delete = FALSE){
    if(is.null(remove)) return(NULL)
    observeEvent(input[[inputId]], {
        reportProgress(inputId, module)
        selectedRow <- getTableActionLinkRow(input, inputId)
        id <- names(data$list)[selectedRow]
        removeRow <- function(...){
            if(delete) {
                path <- data$list[[id]]$path
                if(!is.null(path) && file.exists(path)) unlink(path)
            }
            reportProgress(paste(selectedRow, '=', id))
            if(!is.null(data$clearLocks)) data$clearLocks(id) # only caller knows how to do lock clearing 
            if(!is.null(data$purgeOutput) && serverEnv$IS_LOCAL) purgeOutputFiles(id) # when removing a job, delete its entire directory # nolint
            data$selected(NA)
            data$list[[id]]  <- NULL # cascades to update data$ids via dataListObserver
            data$names[[id]] <- NULL
            if(!is.null(remove$remove)) remove$remove(id)
            if(!is.null(sendFeedback)) sendFeedback(NULL)       
        }
        if(!is.null(remove$confirm) && !remove$confirm) return(removeRow())
        name <- if(is.character(remove$name)) data$list[[id]][[remove$name]] else remove$name(id)        
        showUserDialog(
            if(delete) 'Confirm File Deletion' else 'Confirm Removal',
            tags$p(remove$message),
            tags$p(name),
            size = if(nchar(remove$message) > 100 || nchar(name) > 50) 'm' else 's',
            type = if(delete) 'deleteCancel' else 'okCancel',
            callback = removeRow
        )
    })
}

# edit record human-readable name action
addNameEditObserver <- function(input, inputId, module, data, buffer, parentNS, colI){
    observeEvent(input[[inputId]], {
        reportProgress(inputId, module)
        
        # update our record of display names    
        edit <- getTableEditBoxData(input, inputId)
        dataRowI <- bufferRowToDataRow(buffer, edit$selectedRow)
        reportProgress(paste(edit$selectedRow, '=', dataRowI, '=', edit$newValue))
        id <- data$ids[dataRowI]
        data$names[[id]] <- edit$newValue
    
        # update the table proxy, via the buffer, to ensure continued proper display in UI
        buffer <- buffer()
        buffer[edit$selectedRow, colI] <- getTableEditBox(
            parentNS(inputId),
            edit$selectedRow,
            edit$newValue
        )
        buffer(buffer)
    })
}

#----------------------------------------------------------------------
# help handle resetting of the edit panel
#----------------------------------------------------------------------

# add a generalized reset observer
addResetObserver <- function(inputIds, input, module, data, sendFeedback, updateEditPanel){
    resetEditPanel <- function(message = NULL){
        reportProgress('resetEditPanel', module)
        sendFeedback(message)
        if(is.na(data$selected())){ # both of these actions cascade to update the sample selection grid
            updateEditPanel( updateEditPanel() + 1 )
        } else {
            selectRows(data$proxy, NULL)
        }
    }
    observeEvent({
        for(id in inputIds) input[[id]]
        TRUE # must return true for reset to fire
    }, {
        resetEditPanel()
    })
    resetEditPanel # return the reset function for use in module-specific actions
}
