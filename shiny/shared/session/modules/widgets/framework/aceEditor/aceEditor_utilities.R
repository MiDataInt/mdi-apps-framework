#----------------------------------------------------------------------
# utilities for populating a dialog with a stateful Ace editor
#----------------------------------------------------------------------
aceEditorLink <- function(id, class = NULL, isHeader = TRUE){
    if(is.null(class) && isHeader) class <- "header-link"
    actionLink(id, NULL, icon = icon("code"), class = class)
}

#----------------------------------------------------------------------
# launch a stateful code viewer/editor
#----------------------------------------------------------------------
showAceEditor <- function(
    session, 
    baseDirs = NULL,    # one or more directories from which all files are shown as trees
    showFile = NULL,    # a single target file to show in lieu of baseDirs
    editable = FALSE,   # whether to allow users to edit the files they open
    loaded = NULL,      # a list of files that have been previously opened in this R session
    tall = FALSE,       # whether the dialog is currently extra-large (xl)
    wide = FALSE,
    sourceError = NULL, # when the editor is opened due to a script source error
    sourceErrorType = ""
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
        baseDirs = baseDirs,
        showFile = showFile,
        editable = editable,
        loaded = if(is.null(loaded)) cache$loaded else loaded,
        tabs = cache$tabs,
        tall = if(!is.null(cache$tall)) cache$tall else tall,
        wide = if(!is.null(cache$wide)) cache$wide else wide,
        sourceError = sourceError,
        sourceErrorType = sourceErrorType
    )
    showUserDialog(
        HTML(paste(
            paste("Code", if(editable) "Editor" else "Viewer", if(is.null(sourceError)) "" else "- error sourcing script"), 
            tags$i(
                id = "aceEditorSpinner",
                class = "fas fa-spinner fa-spin",
                style = "margin-left: 2em; color: #3c8dbc; display: none;"
            )
        )), 
        aceEditorUI(
            nsId, 
            baseDirs = baseDirs,
            showFile = showFile,
            baseDir  = cache$baseDir,
            editable = editable
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
initializeAceEditor <- function(editorId, editable){ # initialize the editor itself
    initMessage <- if(editable) "initializeAceCodeEditor" else "initializeAceCodeReader"
    session$sendCustomMessage(initMessage, editorId)
    TRUE
}
initializeAceSession <- function(editorId, path, loaded){ # initialize a file for editing
    contents <- if(is.null(loaded) || !loaded) gsub("\\r", "", loadResourceText(path)) else NULL
    session$sendCustomMessage("initializeAceSession", list(
        editorId = editorId,
        path = path,
        contents = contents
    ))
    contents
}
clearAceSession <- function(editorId){
    session$sendCustomMessage("clearAceSession", editorId)
}
resetSessionContents <- function(editorId, path){
    session$sendCustomMessage("resetSessionContents", list(
        editorId = editorId,
        path = path
    ))
}
terminateAceSession <- function(editorId, closingPath, newPath){ # close a file
    session$sendCustomMessage("terminateAceSession", list(
        editorId = editorId,
        closingPath = closingPath,
        newPath = newPath
    ))
}
