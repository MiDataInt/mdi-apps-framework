#----------------------------------------------------------------------
# reactive components for constructing an Ace code viewer/editor panel
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
aceEditorServer <- function( # generally, you do not call aceEditorServer directly
    id,                      # see showAceEditor()
    baseDirs = NULL,    # one or more directories from which all files are shown as trees
    showFile = NULL,    # a single target file to show in lieu of baseDirs
    editable = FALSE,   # whether to allow users to edit the files they open
    loaded = NULL,      # a list of files that have been previously opened in this R session
    tabs = NULL,        # a data.table of information about the files currently opened in tabs
    tall = FALSE,       # whether the dialog is currently extra-large (xl)
    wide = FALSE,
    sourceError = NULL, # when the editor is opened due to a script source error
    sourceErrorType = ""
){
    moduleServer(id, function(input, output, session){
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize the module
#----------------------------------------------------------------------
aceIsInitialized <- FALSE
module <- "aceEditor"
editorId <- session$ns("ace")
changedId  <- "ace-changed"
contentsId <- "ace-contents"
closeId    <- "ace-close"
nsCloseId  <- session$ns(closeId)
discardId  <- "ace-discard"
nsDiscardId  <- session$ns(discardId)
switchId   <- "ace-switch"
nsSwitchId  <- session$ns(switchId)
spinnerSelector <- "#aceEditorSpinner"
observers <- list() # for module self-destruction
isSingleFile <- !is.null(showFile)
if(is.null(loaded)) loaded <- list()

#----------------------------------------------------------------------
# the file selector tree
#----------------------------------------------------------------------
files <- reactiveValues() # named vectors of files (name = path, value = path relative to baseDir) by baseDir
output$tree <- shinyTree::renderTree({
    req(input$baseDir)
    show(selector = spinnerSelector)
    relPaths <- if(isSingleFile) basename(showFile)
                else list.files(input$baseDir, recursive = TRUE)   
    paths <- file.path(input$baseDir, relPaths)
    names(relPaths) <- paths
    files[[input$baseDir]] <- relPaths
    x <- file.path(basename(input$baseDir), relPaths)
    tree <- data.tree::as.Node(data.frame(pathString = x))
    x <- as.list(tree)
    x$name <- NULL
    if(isSingleFile) isolate({ setActiveTab(paths) })
    hide(selector = spinnerSelector)
    x
})
# respond to a file tree click
observers$tree <- observeEvent(input$tree, {
    req(input$tree)
    req(input$baseDir)
    x <- shinyTree::get_selected(input$tree)
    req(length(x) == 1) # == 0 when not selected; never >0 since multi-select disabled
    x <- x[[1]]    
    x <- paste(c(attr(x, 'ancestry'), x), collapse = "/") # reassemble the file path relative to tree root
    req(x %in% files[[input$baseDir]]) # this line suppresses directories in the tree
    setActiveTab(file.path(input$baseDir, x))
})
# show the full path of the selected file in the editor pane
output$file <- renderText({ # must remove leading "./" for rtl ellipsis to work correctly 
    substring(tabs()[active == TRUE, path], 2) 
})

#----------------------------------------------------------------------
# the file tabs
#----------------------------------------------------------------------
tabs <- reactiveVal(if(is.null(tabs)) data.table(
    path = character(), 
    active = logical(),
    changed = logical(),
    error = logical(),
    message = character()
) else tabs)
setActiveTab <- function(activePath, closingPath = NULL){
    show(selector = spinnerSelector)
    tabs <- tabs()        
    if(!is.null(activePath)){
        tabs$active <- FALSE
        if(activePath %in% tabs$path) tabs[path == activePath, active := TRUE]        
        else tabs <- rbind(tabs, data.table(path = activePath, active = TRUE, 
                                            changed = FALSE, error = FALSE, message = ""))
    }
    if(!aceIsInitialized) aceIsInitialized <<- initializeAceEditor(editorId, editable)
    if(is.null(closingPath)){
        contents <- initializeAceSession(editorId, activePath, loaded[[activePath]])
        loaded[[activePath]] <<- TRUE
    } else {
        contents <- NULL
        terminateAceSession(editorId, closingPath, activePath)
    }
    tabs(tabs) 
    hide(selector = spinnerSelector)         
    checkCodeSyntax(activePath, contents)     
}
invalidateTabs <- reactiveVal(0)
output$tabs <- renderUI({
    invalidateTabs()
    tabs <- tabs()
    req(tabs)
    tagList(
        lapply(seq_len(nrow(tabs)), function(i){
            activeClass  <- if(tabs[i, active])  "aceEditor-tab-active"  else ""
            changedClass <- if(tabs[i, changed]) "aceEditor-tab-changed" else ""
            errorClass   <- if(tabs[i, error])   "aceEditor-tab-error"   else ""
            tags$div(
                class = paste("aceEditor-tab", activeClass),
                tags$span(
                    class = paste("aceEditor-tab-switch", changedClass, errorClass), 
                    basename(tabs[i, path]),
                    onclick = paste0("Shiny.setInputValue('", nsSwitchId, "', '", tabs[i, path], "', { priority: 'event' })") # nolint
                ), 
                if(tabs[i, changed]) tags$i( # show EITHER a file close X or a file save icon
                    class = "aceEditor-tab-save fa fa-hdd-o", 
                    onclick = paste0("saveAceSessionContents('", editorId, "', '", tabs[i, path], "', 'save', { priority: 'event' })") # nolint
                ) else "",
                if(!isSingleFile && !tabs[i, changed]) tags$i(
                    class = "aceEditor-tab-close fa fa-times", 
                    onclick = paste0("Shiny.setInputValue('", nsCloseId, "', '", tabs[i, path], "', { priority: 'event' })") # nolint
                ) else ""
            )
        }),
        if(isSingleFile  && tabs[1, changed]) tags$a(
            "Discard Changes", 
            onclick = paste0("resetSessionContents('", editorId, "', '", tabs[1, path], "', { priority: 'event' })"),
            style = "margin-left: 15px; cursor: pointer;"
        ) else ""
    )
})
observers$switchId <- observeEvent(input[[switchId]], {
    newPath <- input[[switchId]] # when a currently inactive tab is clicked
    req(newPath)
    activePath <- tabs()[active == TRUE, path]
    req(activePath)
    req(newPath != activePath)
    setActiveTab(newPath)
})
observers$closeId <- observeEvent(input[[closeId]], {
    closingPath <- input[[closeId]] # the X icon that close files AND discards all changes
    req(closingPath)
    tabs <- tabs()    
    closingI <- tabs[path == closingPath, .I]
    isChanged <- tabs[path == closingPath, changed]
    tabs <- tabs[path != closingPath]
    if(nrow(tabs) > 0){
        newI <- if(closingI > 1) closingI - 1 else 1  
        newPath <- tabs[newI, path]
    } else {
        newPath <- NULL
    }
    tabs(tabs)
    loaded[closingPath] <<- FALSE
    setActiveTab(newPath, closingPath)
})
observers$discardId <- observeEvent(input[[discardId]], {
    tabs <- tabs()   # used in single-file mode only in lieu of X icon
    req(tabs)  
    path <- tabs[1, path]
    req(path)
    checkCodeSyntax(path, input[[discardId]])
    tabs[1, changed := FALSE]
    tabs(tabs)
    setActiveTab(path)
    invalidateError(invalidateError() + 1)
    invalidateTabs(invalidateTabs() + 1)
})

#---------------------------------------------------------------------
# the Ace editor
#----------------------------------------------------------------------
# monitor files for changed status (a simple flag, does not receive editor contents on every keystroke)
observers$changedId <- observeEvent(input[[changedId]], {
    tab <- input[[changedId]]
    tabs <- tabs()
    tabs[path == tab$path, changed := tab$changed]
    tabs(tabs)
    invalidateTabs(invalidateTabs() + 1)
})
# save the contents of changed files when the file save icon is clicked
observers$contentsId <- observeEvent(input[[contentsId]], { 
    tab <- input[[contentsId]]
    tab$contents <- gsub("\\r", "", tab$contents)
    checkCodeSyntax(tab$path, tab$contents)
    if(tab$action == "save") {
        tabs <- tabs()
        if(!tabs[path == tab$path, error]){
            cat(tab$contents, file = tab$path)    
            tabs[path == tab$path, changed := FALSE]
            tabs(tabs)
        }  
    }     
    invalidateError(invalidateError() + 1)
    invalidateTabs(invalidateTabs() + 1)
})

#----------------------------------------------------------------------
# toggle the editor dimensions
#----------------------------------------------------------------------
toggleSize <- function(){
    toggleClass(selector = ".modal-dialog", class = "modal-xl", condition = wide)
    toggleClass(selector = ".ace-editor-lg", class = "ace-editor-xl", condition = tall)
    toggleClass(selector = ".ace-editor-tree-lg", class = "ace-editor-tree-xl", condition = tall)
    aceIsInitialized <<- initializeAceEditor(editorId, editable)
}
observers$toggleWidth <- observeEvent(input$toggleWidth, { 
    wide <<- !wide
    toggleSize()
})
observers$toggleHeight <- observeEvent(input$toggleHeight, { 
    tall <<- !tall    
    toggleSize()
})

#----------------------------------------------------------------------
# report syntax errors for supported languages; intermittent, not dynamic
#----------------------------------------------------------------------
noSyntax <- "syntax checking not supported"
noErrors <- "no syntax errors detected"
supportedExtensions <- c("R", "yml")
checkCodeSyntax <- function(path_, contents){
    req(contents)
    extension <- rev(strsplit(path_, "\\.")[[1]])[1]
    tabs <- tabs()
    tabs[path == path_, message := tryCatch({
        if(extension %in% supportedExtensions){
            invisible(switch(
                extension,
                R   = parse(text = contents),
                yml = read_yaml(text = contents)
            ))
            noErrors
        } else {
            noSyntax
        }
    }, error = function(e) trimws(e$message))]
    tabs[path == path_, error := !(message %in% c(noSyntax, noErrors))]
    tabs(tabs)
}
observers$checkSyntax <- observeEvent(input$checkSyntax, { # (Re)Check Syntax link click
    tabs <- tabs()
    req(tabs)
    path <- tabs[active == TRUE, path]
    req(path)
    runjs(paste0("saveAceSessionContents('", editorId, "', '", path, "', 'syntax', { priority: 'event' })"))
})
invalidateError <- reactiveVal(0)
output$error <- renderText({
    invalidateError()
    if(!is.null(sourceError)){ # one-time display of an initial R script source error
        message <- sourceError$message
        sourceError <<- NULL
        return(message)
    }
    tabs <- tabs()
    req(tabs)
    message <- tabs[active == TRUE, message]
    req(message)
    message
})

#----------------------------------------------------------------------
# restore state on first load
#----------------------------------------------------------------------
initStateTrigger <- reactiveVal(1)
initState <- observeEvent(initStateTrigger(), {
    if(nrow(tabs()) > 0) setActiveTab(tabs()[active == TRUE, path])
    toggleSize()
    initStateTrigger <<- NULL
    initState$destroy()
})

#----------------------------------------------------------------------
# return value
#----------------------------------------------------------------------
sourceErrorType # required, otherwise lazy evaluation won't propagate value into onDestroy
list(
    observers = observers, # for use by destroyModuleObservers
    onDestroy = function() {
        if(sourceErrorType == "framework"){
            runjs("location.reload();")
            return(NULL)
        } else if(sourceErrorType == "app"){
            retryLoadRequest( retryLoadRequest() + 1 )
            return(NULL)
        } else {
            reloadAllAppScripts(session, app)
        }
        list(  # return the module's cached state object
            baseDir = input$baseDir, # used by UI
            loaded = loaded, # used by server          
            tabs = tabs(),
            tall = tall,
            wide = wide
        )               
    }
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
