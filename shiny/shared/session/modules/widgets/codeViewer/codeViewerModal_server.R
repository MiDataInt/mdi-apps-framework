#----------------------------------------------------------------------
# reactive components that provide a modal code viewer for a specific file
# TODO: enable Edit+Save when in developer mode and working with developer-forks
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
codeViewerModalServer <- function(
    id, 
    parentId,
    codeFile # string, reactive, or function with no arguments that return a code file path
) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        parentNs <- NS(parentId)
        module <- 'codeViewerModal' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# setup module with common results access elements
#----------------------------------------------------------------------
editorId <- "modalCodeEditor"
editorContentsId <- paste(editorId, "contents", sep = "-") # for JS-Shiny communication
editorIsInitialized <- FALSE
diskFileContents <- reactiveVal(NULL) # as found on disk when first loaded
currentFileContents <- reactiveVal(NULL) # as potentially edited by the user

#----------------------------------------------------------------------
# open a modal with an ACE editor loaded with codeFile
#----------------------------------------------------------------------
observeEvent(input$showCode, {
    req(codeFile)
    codeFile <- if(is.character(codeFile)) codeFile else codeFile()
    req(codeFile)
    showModal(modalDialog(
        title = "Code Viewer",
        tags$div(
            tags$p(tags$strong(codeFile)),
            tags$div(
                id = editorId,
                style = "height: 600px;"
            )            
        ),
        easyClose = TRUE,
        footer = NULL,
        size = "l",
        fade = serverEnv$IS_LOCAL_BROWSER && !serverEnv$IS_DEVELOPER
    ))

    # initialize the editor
    session$sendCustomMessage("initializeAceCodeReader", editorId)

    # load the contents of a config file for the first time
    if(is.null(currentFileContents())){
        x <- loadResourceText(codeFile)  
        diskFileContents(gsub("\\r", "", x))
        currentFileContents(diskFileContents())
    }

    # set the working config file contents into the editor
    session$sendCustomMessage("setAceCodeContents", list(
        editorId = editorId,
        code = currentFileContents()
    ))
})

# module return value
NULL

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
