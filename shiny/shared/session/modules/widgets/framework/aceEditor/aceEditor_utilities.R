#----------------------------------------------------------------------
# utilities for populating a dialog with a stateful Ace editor
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# launch a stateful code viewer/editor
#----------------------------------------------------------------------
aceEditorCache <- list()
showAceEditor <- function(
    session, 
    options = list(
        baseDir   = character(),
        editable  = FALSE,
        fileTree  = FALSE,
        multiPane = FALSE
    )
){
    id <- "aceEditorDialog"
    nsId <- session$ns(id)
    cache <- aceEditorCache[[nsId]]
    ns <- NS(nsId)
    onExit <- function(...){
        removeMatchingInputValues(session, id, exclude = ns("baseDir")) # not in use, disables input$baseDir
        aceEditorCache[[nsId]] <<- destroyModuleObservers(aceEditorCache[[nsId]])   
    }
    aceEditorCache[[nsId]] <<- aceEditorServer(
        id, 
        options = options,
        state = cache$state,
        onExit = onExit
    )
    isOption <- function(x) !is.null(options[[x]]) && options[[x]]
    showUserDialog(
        HTML(paste(
            paste("Code", if(isOption("editable")) "Editor" else "Viewer"), 
            tags$i(
                id = "aceEditorSpinner",
                class = "fas fa-spinner fa-spin",
                style = "margin-left: 2em; color: #3c8dbc; display: none;"
            )
        )), 
        aceEditorUI(
            nsId, 
            options = options,
            state = cache$state
        ),
        size = "l", 
        type = 'dismissOnly', 
        easyClose = FALSE,
        fade = FALSE,
        callback = onExit
    )
}

#----------------------------------------------------------------------
# editor support functions
#----------------------------------------------------------------------
initializeAceEditor <- function(editorId, editable){ # initalize the editor itself
    initMessage <- if(editable) "initializeAceCodeEditor" else "initializeAceCodeReader"
    session$sendCustomMessage(initMessage, editorId)
    TRUE
}
initializeAceSession <- function(editorId, path, contents){ # initialize a file for editing
    isNew <- is.null(contents)
    if(isNew) contents <- loadResourceText(path)
    session$sendCustomMessage("initializeAceSession", list(
        editorId = editorId,
        path = path,
        contents = if(isNew) contents else NULL
    ))
    contents
}
getAceSessionContents <- function(editorId, path){
    session$sendCustomMessage("getAceSessionContents", list(
        editorId = editorId,
        path = path
    ))
}
terminateAceSession <- function(editorId, closingPath, newPath){ # close a file
    dprint(paste("terminateAceSession", closingPath, newPath))
    session$sendCustomMessage("terminateAceSession", list(
        editorId = editorId,
        closingPath = closingPath,
        newPath = newPath
    ))
}
