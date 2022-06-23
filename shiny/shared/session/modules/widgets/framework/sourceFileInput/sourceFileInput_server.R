#----------------------------------------------------------------------
# reactive components for sample source file upload
# handles all types including manifests, package zips, bookmarks and data tables
# used on launch page and in sourceFileUpload module
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
sourceFileInputServer <- function(
    id, 
    appName = NULL, 
    externalSuffixes = c(), 
    createButtonServer = NULL
) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id)
        module <- ns('sourceFileInput') # for reportProgress tracing

#----------------------------------------------------------------------
# initialize the widget
#----------------------------------------------------------------------

# set our return value
incomingFile <- reactiveVal(list(
    name = NULL,
    path = NULL,
    type = NULL,
    suppressUnlink = NULL
))

# customize the file upload feedback
sendFeedback <- recordFeedbackFunction(output, 'fileInputFeedback')
allowedFileTypes <- getAllowedSourceFileTypes(appName, externalSuffixes)
isLaunchPage <- is.null(appName)

#----------------------------------------------------------------------
# enable the local file upload input (working from right to left...)
#----------------------------------------------------------------------
handleIncomingSourceFile <- function(file, suppressUnlink = FALSE){
    reportProgress('handleIncomingSourceFile')
    req(file)
    req(is.list(file))
    req(nrow(file) == 1)
    loadIncomingFile(
        file = file,
        allowedFileTypes = allowedFileTypes,
        sendFeedback = sendFeedback,
        isLaunchPage = isLaunchPage,
        incomingFile = incomingFile,
        suppressUnlink = suppressUnlink
    )
}
observeEvent(input$fileInput, {
    file <- input$fileInput
    req(file)    
    reportProgress('input$fileInput', module)
    handleIncomingSourceFile(file)
})

#----------------------------------------------------------------------
# as needed, enable the server-side file browser
#----------------------------------------------------------------------
if(exposeServerFiles()) {
    serverSourceFilesButtonServer(
        'serverFileInput', 
        input, 
        session, 
        rw = "read", 
        filetypes = c("yml", "mdi", "zip", "csv"),
        loadFn = function(file) handleIncomingSourceFile(file, suppressUnlink = TRUE)
    ) 
}

#----------------------------------------------------------------------
# as needed, enable the create new button with actions defined by caller
#----------------------------------------------------------------------
createButtonData <- if(!is.null(createButtonServer)) createButtonServer('createNew', id) else NULL

#----------------------------------------------------------------------
# return reactive with path to incoming file
#----------------------------------------------------------------------
list(
    file = incomingFile,
    sendFeedback = sendFeedback,
    createButtonData = createButtonData
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
