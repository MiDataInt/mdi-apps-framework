#----------------------------------------------------------------------
# handlers for specific types of modal popups
#----------------------------------------------------------------------

# at session start, listen for requests to close modal popups
addRemoveModalObserver <- function(input){
    observeEvent(input$removeModal, {
        removeModal() # a naive Shiny function
        if(!is.null(modalTmpFile)) { # remove and tmp files called by URL (e.g. an HTML report)
            unlink(modalTmpFile, force = TRUE)
            modalTmpFile <<- NULL
        }
    }) 
}

# a large format modal popup that shows an HTML document
showHtmlModal <- function(file, type, title){

    # copy to www/tmp for direct access via URL
    startSpinner(session, 'showHtmlModal')
    tmpName <- paste(type, 'html', sep = ".")
    modalTmpFile <<- file.path(sessionDirectory, tmpName)
    file.copy(file, modalTmpFile, overwrite = TRUE)
    src <- paste(sessionUrlBase, tmpName, sep = '/')

    # open the report in an iframe, in a modal, as big as possible
    showModal(modalDialog(
        title = tagList(
            tags$span(title),
            bsButton('removeModal', "Dismiss", style = "primary", class = "modal-dismiss-button")
        ),
        tags$iframe(
            src = src,
            scrolling = 'yes',
            seamless = TRUE,
            width = '100%',
            height = '100%'
        ),
        easyClose = FALSE, # must use our dismiss button, which deletes the tmp file
        footer = NULL,
        size = "l", # sets fixed width
        class = 'full-screen-modal', # sets fixed height,
        fade = serverEnv$IS_LOCAL_BROWSER
    ))
    stopSpinner(session)
}

# a small format modal popup for getting user input or confirmation
dialogCallback <- function(parentInput) NULL
showUserDialog <- function(title, ..., callback=function(parentInput) NULL,
                           size="s", type='okCancel', footer=NULL, easyClose=TRUE){
    dialogCallback <<- callback
    footer <- switch(type,
        okOnly = tagList( # an "information only" dialog
            modalButton("OK")
        ),
        okCancel = tagList( # an action that require input and/or confirmation
            modalButton("Cancel"),
            actionButton("userDialogOk", "OK")
        ),
        deleteCancel = tagList( # an action that require input and/or confirmation
            modalButton("Cancel"),
            bsButton("userDialogOk", "Delete Permanently", style = "warning")
        ),
        discardCancel = tagList( # an action that require input and/or confirmation
            modalButton("Cancel"),
            bsButton("userDialogOk", "Discard Permanently", style = "warning")
        ),
        okOnlyWithAction = tagList( # an information dialog that executes an action upon closing
            actionButton("userDialogOk", "OK")
        ),
        custom = footer, # the caller provides all the buttons
        ""
    )
    stopSpinner(session)
    showModal(modalDialog(
        title = tags$strong(title),
        ...,
        tags$div(id = 'modal-dialog-error', ''),
        easyClose = easyClose, # allow easy dismissal
        footer = footer,
        size = size, # sets fixed width, height auto-adjusts
        fade = serverEnv$IS_LOCAL_BROWSER
    ))
}
observeEvent(input$userDialogOk, {
    tryCatch({
        dialogCallback(input)
        removeModal()
    }, error = function(e){
        runjs(paste0( '$("#modal-dialog-error").html("', e$message, '")' ))
    })
})

# a modal for pending items in a development environment
showPendingDialog <- function(){
   showUserDialog(
        'Pending',
        'action/item is under development',
        type = 'okOnly'
    )
}
