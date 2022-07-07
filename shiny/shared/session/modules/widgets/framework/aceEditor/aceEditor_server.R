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
    onExit = NULL       # function called when the dialog is dismissed
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
    error = logical()
) else tabs)
setActiveTab <- function(activePath, closingPath = NULL){
    show(selector = spinnerSelector)
    tabs <- tabs()
    if(!is.null(activePath)){
        tabs$active <- FALSE
        if(activePath %in% tabs$path) tabs[path == activePath, active := TRUE]        
        else tabs <- rbind(tabs, data.table(path = activePath, active = TRUE, 
                                            changed = FALSE, error = FALSE))
    }
    if(!aceIsInitialized) aceIsInitialized <<- initializeAceEditor(editorId, editable)
    if(is.null(closingPath)){
        initializeAceSession(editorId, activePath, loaded[[activePath]])
        loaded[[activePath]] <<- TRUE
    } else {
        terminateAceSession(editorId, closingPath, activePath)
    }
    tabs(tabs)  
    hide(selector = spinnerSelector)
}
invalidateTabs <- reactiveVal(0)
output$tabs <- renderUI({
    invalidateTabs()
    tabs <- tabs()
    req(tabs)
    lapply(seq_len(nrow(tabs)), function(i){
        activeClass  <- if(tabs[i, active])  "aceEditor-tab-active" else ""
        changedClass <- if(tabs[i, changed]) "aceEditor-tab-changed" else ""
        errorClass   <- if(tabs[i, error])   "aceEditor-tab-error" else ""
        tags$div(
            class = paste("aceEditor-tab", activeClass, changedClass, errorClass), 
            if(tabs[i, changed]) tags$i(
                class = "aceEditor-tab-save fa fa-hdd-o", 
                onclick = paste0("saveAceSessionContents('", editorId, "', '", tabs[i, path], "', { priority: 'event' })")
            ) else "",
            tags$span(
                class = "aceEditor-tab-switch", 
                basename(tabs[i, path]),
                onclick = paste0("Shiny.setInputValue('", nsSwitchId, "', '", tabs[i, path], "', { priority: 'event' })")
            ), 
            tags$i(
                class = "aceEditor-tab-close fa fa-times", 
                onclick = paste0("Shiny.setInputValue('", nsCloseId, "', '", tabs[i, path], "', { priority: 'event' })")
            )
        )
    })
})
observers$switchId <- observeEvent(input[[switchId]], {
    newPath <- input[[switchId]]
    req(newPath)
    activePath <- tabs()[active == TRUE, path]
    req(activePath)
    req(newPath != activePath)
    setActiveTab(newPath)
})
observers$closeId <- observeEvent(input[[closeId]], {
    closingPath <- input[[closeId]]
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
    setActiveTab(newPath, closingPath)
})

#---------------------------------------------------------------------
# the Ace editor
#----------------------------------------------------------------------
# monitor files for changed status
observers$changedId <- observeEvent(input[[changedId]], {
    tab <- input[[changedId]]
    tabs <- tabs()
    tabs[path == tab$path, changed := tab$changed]
    tabs(tabs)
    invalidateTabs(invalidateTabs() + 1)
})
# save the contents of changed files when the file save icon is clicked
observers$contentsId <- observeEvent(input[[contentsId]], { 
    show(selector = spinnerSelector)
    tab <- input[[contentsId]]
    tabs <- tabs()
    cat(tab$contents, file = tab$path)
    tabs[path == tab$path, changed := FALSE]
    tabs(tabs)
    hide(selector = spinnerSelector)    
    invalidateTabs(invalidateTabs() + 1)
})

#----------------------------------------------------------------------
# toggle the editor dimensions
#----------------------------------------------------------------------
toggleSize <- function(){
    toggleClass(selector = ".modal-dialog", class = "modal-xl", condition = wide)
    toggleClass(selector = ".ace-editor-lg", class = "ace-editor-xl", condition = tall)
    toggleClass(selector = ".ace-editor-tree-lg", class = "ace-editor-tree-xl", condition = tall)
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
# restore state on first load
#----------------------------------------------------------------------
initState <- observeEvent(tabs(), {
    if(nrow(tabs()) > 0) setActiveTab(tabs()[active == TRUE, path])
    toggleSize()
    initState$destroy()
})

#----------------------------------------------------------------------
# return value
#----------------------------------------------------------------------
list(
    observers = observers, # for use by destroyModuleObservers
    onDestroy = function() {
        reloadAllAppScripts(session, app)
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
