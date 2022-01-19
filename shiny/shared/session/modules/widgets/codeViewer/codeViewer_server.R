#----------------------------------------------------------------------
# reactive components that provide a code viewer for page-level MDI scripts
# TODO: enable Edit+Save when in developer mode and working with developer-forks
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
codeViewerServer <- function(
    id, 
    parentId, # typically an appStep id
    showApp = TRUE # if TRUE, also show root-level app scripts
) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        parentNs <- NS(parentId)
        module <- 'codeViewer' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# discover the directories that carry code relevant to a given app and appStep
#----------------------------------------------------------------------
appDir_ <- reactive({
    req(showApp)
    app$DIRECTORY
})
modules <- 'modules'
sharedModules <- file.path('shiny', 'shared', 'session', modules)
appSteps <- 'appSteps'
appStepDir_ <- reactive({ # check app step directories in order of their precedence
    appStep <- app$config$appSteps[[parentId]]
    req(appStep)
    moduleName <- appStep$module
    dir <- file.path(app$DIRECTORY, modules, appSteps, moduleName) # first look in app modules directory
    if(dir.exists(dir)) return(dir)
    dir <- file.path(gitStatusData$suite$dir, sharedModules, appSteps, moduleName) # next look in suite shared modules
    if(dir.exists(dir)) return(dir)
    dir <- file.path(gitFrameworkStatus$dir, sharedModules, appSteps, moduleName) # finally try framework shared modules
    if(dir.exists(dir)) return(dir)
    NULL
})

#----------------------------------------------------------------------
# setup module with common results access elements
#----------------------------------------------------------------------
editorId <- paste(parentNs(id), "editor", sep = "-") # the div that contains the Ace editor
editorContentsId <- paste("editor", "contents", sep = "-") # for JS-Shiny communication
editorIsInitialized <- FALSE
codeFiles <- reactiveVal(character()) # character vector of code file paths
diskFileContents <- reactiveValues() # as found on disk when first loaded
currentFileContents <- reactiveValues() # as potentially edited by the user
workingFile <- reactiveVal(NULL) # the member of codeFiles() currently displayed in the editor

#----------------------------------------------------------------------
# enable the code display toggle
#----------------------------------------------------------------------
codeIsVisible <- reactiveVal(FALSE)
observeEvent(input$toggleCode, {
    condition <- !codeIsVisible()
    codeIsVisible(condition)
    toggle('codeViewerDiv', condition = condition)
    updateActionLink(session, 'toggleCode', if(condition) "Hide Code" else "Show Code")
})

#----------------------------------------------------------------------
# show the paths where we are viewing/editing files
#----------------------------------------------------------------------
simplifyDir <- function(x) {
    req(x)
    x <- gsub(file.path(app$DIRECTORY, modules), '<app modules>', x)
    x <- gsub(file.path(gitStatusData$suite$dir, sharedModules), '<suite modules>', x) 
    x <- gsub(file.path(gitFrameworkStatus$dir,  sharedModules), '<mdi modules>', x)
    x <- gsub(app$DIRECTORY, paste0('<app = ', app$NAME, '>'), x) 
    x <- gsub(gitStatusData$suite$dir, '<suite>', x) 
    x <- gsub(gitFrameworkStatus$dir, '<framework>', x) 
    x
}
output$appStepPath <- renderText({
    simplifyDir(appStepDir_())
})
output$appPath <- renderText({
    simplifyDir(appDir_())
})
output$workingFile <- renderText({
    workingFile()
})

#----------------------------------------------------------------------
# file selector tree
#----------------------------------------------------------------------

# render the file trees; one level deep
getFileTree <- function(dir){
    req(dir)
    if(!editorIsInitialized){ # initialize editor; must be done after the UI is created
        session$sendCustomMessage("initializeAceCodeReader", editorId)
        editorIsInitialized <<- TRUE
    }
    fileNames <- list.files(dir, pattern = "\\.yml|\\.R|\\.md", recursive = FALSE)
    isolate({ codeFiles(c(codeFiles(), file.path(dir, fileNames))) })
    fileNames <- as.list(fileNames)
    names(fileNames) <- fileNames # shinyTree requires a named list
    fileNames
}
output$appStepTree <- shinyTree::renderTree({
    getFileTree(appStepDir_())
})
output$appTree <- shinyTree::renderTree({
    getFileTree(appDir_())
})

# respond to a file tree click
addTreeObserver <- function(id, dir){
    req(dir)
    observeEvent(input[[id]], {

        # get the file we will switch to
        req(input[[id]])
        x <- shinyTree::get_selected(input[[id]])
        req(length(x) == 1) # == 0 when not selected; never >0 since multi-select disabled
        x <- x[[1]]
        x <- file.path(dir, x) # reassemble the file path relative to tree root
        req(x %in% codeFiles())

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
            diskFileContents[[x]] <- loadResourceText(x)  
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
}
addTreeObserver('appStepTree', appStepDir_())
addTreeObserver('appTree', appDir_())

# ----------------------------------------------------------------------
# cache potentially edited file contents when switching files
# ----------------------------------------------------------------------
observeEvent(input[[editorContentsId]], {
    x <- input[[editorContentsId]]
    currentFileContents[[x$file]] <- x$code
    # if(!is.null(x$flag) && x$flag == "save") saveConfigFiles()
})

# #----------------------------------------------------------------------
# # set return value as a save function
# # must cascade this way so that the current editor window is also retrieved and examined
# #----------------------------------------------------------------------
# saveConfigFiles <- function(){
#     configChanged <- FALSE
#     installationRequired <- FALSE
#     for(filename in names(diskFileContents)){
#         disk   <- paste0(trimws(diskFileContents[[filename]]),    "\n\n")
#         edited <- paste0(trimws(currentFileContents[[filename]]), "\n\n")
#         if(disk != edited){
#             configChanged <- TRUE
#             if(filename == "suites.yml") installationRequired <- TRUE
#             cat(gsub("\\r", "", edited), file = file.path(appStepDir_(), filename))
#         }   
#     }
#     if(configChanged){
#         showUserDialog(
#             "Server Restart Required", 
#             tags$p(paste(
#                 "The server configuration has changed, the server must",
#                 if(installationRequired) "reinstall and" else "",
#                 "restart now."
#             )),
#             tags$p("Please reload a fresh web page to start a new session once the server restarts."),
#             callback = function(...) {
#                 if(installationRequired){
#                     Sys.setenv(MDI_FORCE_RESTART = "TRUE")
#                     Sys.setenv(MDI_FORCE_REINSTALLATION = "TRUE")
#                 }
#                 stopApp()
#             },
#             size = "s", 
#             type = 'okOnlyCallback', 
#             footer = NULL, 
#             easyClose = TRUE
#         )
#     }
# }
# list(
#     save = function(...){ # this function is the Save button callback on the modal popup
#         if(is.null(workingFile())) return(NULL)
#         session$sendCustomMessage( # get the current editor contents first, they may have changed
#             "getAceCodeContents", 
#             list(
#                 editorId = editorId, 
#                 file = workingFile(),
#                 flag = "save" # cascade to saveConfigFiles after editor content cached in R
#             )
#         )
#     }
# )

NULL

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
