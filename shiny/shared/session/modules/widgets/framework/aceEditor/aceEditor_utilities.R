#----------------------------------------------------------------------
# utilities for populating a dialog with a stateful Ace editor
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# launch a stateful code viewer/editor
#----------------------------------------------------------------------
aceEditorCache <- list()
showAceEditor <- function(
    session, 
    baseDirs = NULL,    # one or more directories from which all files are shown as trees
    showFile = NULL,    # a single target file to show in lieu of baseDirs
    editable  = FALSE,  # whether to allow users to edit the files they open
    loaded = NULL,      # a list of files that have been previously opened in this R session
    tabs = NULL,        # a data.table of information about the files currently opened in tabs
    tall = FALSE,       # whether the dialog is currently extra-large (xl)
    wide = FALSE
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
        loaded = cache$loaded,
        tabs = cache$tabs,
        tall = if(!is.null(cache$tall)) cache$tall else tall,
        wide = if(!is.null(cache$wide)) cache$wide else wide,
        onExit = onExit
    )
    showUserDialog(
        HTML(paste(
            paste("Code", if(editable) "Editor" else "Viewer"), 
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
            baseDir  = cache$baseDir
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
    session$sendCustomMessage("initializeAceSession", list(
        editorId = editorId,
        path = path,
        contents = if(!is.null(loaded) && loaded) NULL else loadResourceText(path)
    ))
}
getAceSessionContents <- function(editorId, path){
    session$sendCustomMessage("getAceSessionContents", list(
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
