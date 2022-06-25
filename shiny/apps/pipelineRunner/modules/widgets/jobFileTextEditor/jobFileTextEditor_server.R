#----------------------------------------------------------------------
# reactive components for text editing a job configuration file
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
jobFileTextEditorServer <- function(id, editMode, activeJobFile){
    moduleServer(id, function(input, output, session){
        module <- 'jobFileTextEditor' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize the editor
#----------------------------------------------------------------------
editorId <- session$ns("editor")
editorContentsId <- "editor-contents"
editorIsInitialized <- FALSE
state <- list(
    disk    = reactiveValues(), # the file contents present at the last load of the file, prior to save
    working = reactiveValues(), # the file contents updated with any user changes in script editor
    pending = reactiveValues()  # whether each file has changes that need to be saved 
)
output$editorFile <- renderText({ 
    req(activeJobFile())
    activeJobFile()$path
})
setEditorContents <- function(path){
    isolate({ session$sendCustomMessage("setAceCodeContents", list(
        editorId = editorId,
        code = state$working[[path]]
    )) })
}

#---------------------------------------------------------------------
# initialize the editor
#----------------------------------------------------------------------
observeEvent({
    editMode()
    activeJobFile()
}, {
    editMode <- editMode()
    req(editMode)
    req(editMode == "editor")
    if(!editorIsInitialized){
        session$sendCustomMessage("initializePRCodeEditor", editorId)
        editorIsInitialized <<- TRUE
    }
    activeJobFile <- activeJobFile()
    req(activeJobFile)
    path <- activeJobFile$path
    req(path)
    if(is.null(state$disk[[path]])){
        startSpinner(session, "loadJobContents")
        code <- loadResourceText(path)
        code <- gsub("\\r", "", code)
        state$disk[[path]] <- code
        state$working[[path]] <- code
        state$pending[[path]] <- NULL
        stopSpinner(session, "loadJobContents")
    }
    setEditorContents(path)
})
observeEvent(input[[editorContentsId]], {
    path <- activeJobFile()$path
    req(path)
    state$working[[path]] <- input[[editorContentsId]]$contents
    state$pending[[path]] <- state$disk[[path]] != state$working[[path]]
})

#----------------------------------------------------------------------
# return value
#----------------------------------------------------------------------
list(
    state = state,
    pending = function(path) {
        if(is.null(state$pending[[path]])) FALSE else state$pending[[path]]
    },
    write = function(newPath, oldPath){
        cat(state$working[[oldPath]], file = newPath)
    },
    save = function(path){
        state$disk[[path]] <- state$working[[path]]
        state$pending[[path]] <- NULL
    },
    saveAs = function(newPath, oldPath){
        state$working[[oldPath]] <- state$disk[[oldPath]]
        state$pending[[oldPath]] <- NULL
    },
    discard = function(path){
        state$working[[path]] <- state$disk[[path]]
        state$pending[[path]] <- NULL
        setEditorContents(path)
    }
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
