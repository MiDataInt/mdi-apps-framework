#----------------------------------------------------------------------
# reactive components that provide a code editor for MDI installation config files
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
configFileEditorServer <- function(id, parentId) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        parentNs <- NS(parentId)
        module <- 'configFileEditor' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# setup module with common results access elements
#----------------------------------------------------------------------
editorId <- paste(parentNs(id), "editor", sep = "-") # the div that contains the Ace editor
editorContentsId <- paste("editor", "contents", sep = "-") # for JS-Shiny communication
editorIsInitialized <- reactiveVal(FALSE)
baseDir <- file.path(serverEnv$MDI_DIR, 'config')
configFiles <- reactiveVal(NULL) # character vector of config file names (not paths)
diskFileContents <- reactiveValues() # as found on disk when first loaded
currentFileContents <- reactiveValues() # as potentially edited by the user
workingFile <- reactiveVal(NULL) # the member of configFile() currently displayed in the editor

#----------------------------------------------------------------------
# initialize file selector tree
#----------------------------------------------------------------------

# render the file tree; one level deep, i.e., just the MDI config files
output$fileTree <- shinyTree::renderTree({
    if(!editorIsInitialized()){ # initialize editor; must be done after the UI is created
        session$sendCustomMessage("initializeAceCodeEditor", editorId)
        editorIsInitialized(TRUE)
    }
    configFiles(list.files(baseDir, pattern = "\\.yml", recursive = FALSE))
    x <- as.list(configFiles())
    names(x) <- x # shinyTree requires a named list
    x
})

# respond to a file tree click
observeEvent(input$fileTree, {

    # get the file we will switch to
    req(input$fileTree)
    x <- shinyTree::get_selected(input$fileTree)
    req(length(x) == 1) # == 0 when not selected; never >0 since multi-select disabled
    x <- x[[1]]
    x <- paste(c(attr(x, 'ancestry'), x), collapse = "/") # reassemble the file path relative to tree root
    req(x %in% configFiles())

    # cache the potentially edited contents of any current working file
    if(!is.null(workingFile())){
        session$sendCustomMessage(
            "getAceCodeContents", 
            list(
                editorId = editorId, 
                file = workingFile()
            )
        )
    }

    # load the contents of a config file for the first time
    if(is.null(currentFileContents[[x]])){
        diskFileContents[[x]] <- loadResourceText(file.path(baseDir, x))  
        diskFileContents[[x]] <- gsub("\\r", "", diskFileContents[[x]])
        currentFileContents[[x]] <- diskFileContents[[x]]
    }

    # set the working config file contents into the editor
    session$sendCustomMessage("setAceCodeContents", list(
        editorId = editorId,
        code = currentFileContents[[x]]
    ))
    workingFile(x)
})

# ----------------------------------------------------------------------
# cache potentially edited file contents when switching files
# ----------------------------------------------------------------------
observeEvent(input[[editorContentsId]], {
    x <- input[[editorContentsId]]
    currentFileContents[[x$file]] <- x$code
    if(!is.null(x$flag) && x$flag == "save") saveConfigFiles()
})

#----------------------------------------------------------------------
# set return value as a save function
# must cascade this way so that the current editor window is also retrieved and examined
#----------------------------------------------------------------------
saveConfigFiles <- function(){
    configChanged <- FALSE
    for(filename in names(diskFileContents)){
        disk   <- paste0(trimws(diskFileContents[[filename]]),    "\n\n")
        edited <- paste0(trimws(currentFileContents[[filename]]), "\n\n")
        if(disk != edited){
            configChanged <- TRUE
            cat(gsub("\\r", "", edited), file = file.path(baseDir, filename))
        }   
    }
    # TODO: need to provide a confirmation modal to user
    if(configChanged) stopApp()
}
list(
    save = function(...){ # this function is the Save button callback on the modal popup
        if(is.null(workingFile())) return(NULL)
        session$sendCustomMessage( # get the current editor contents first, they may have changed
            "getAceCodeContents", 
            list(
                editorId = editorId, 
                file = workingFile(),
                flag = "save" # cascade to saveConfigFiles after editor content cached in R
            )
        )
    }
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
