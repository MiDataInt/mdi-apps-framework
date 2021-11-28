#----------------------------------------------------------------------
# reactive components for sample source file upload
# handles all types including manifests, package zips, bookmarks and data tables
# used on launch page and in sourceFileUpload module
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
sourceFileInputServer <- function(id, appName = NULL, externalSuffixes = c()) {
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
    type = NULL
))

# customize the file upload feedback
sendFeedback <- recordFeedbackFunction(output, 'fileInputFeedback')
allowedFileTypes <- getAllowedSourceFileTypes(appName, externalSuffixes)
isLaunchPage <- is.null(appName)

# as needed, enable the server-side file browser
if(serverEnv$IS_SERVER && isAuthorizedUser()) 
    serverFilesButtonServer('serverFileInput', input, session, 
                            rw = "read") # , filetypes = c("mdi", "csv")

#----------------------------------------------------------------------
# enable the file upload input
#----------------------------------------------------------------------
observeEvent(input$fileInput, {
    file <- input$fileInput
    req(file)    
    reportProgress('input$fileInput', module)
    loadIncomingFile(
        file = file,
        allowedFileTypes = allowedFileTypes,
        sendFeedback = sendFeedback,
        isLaunchPage = isLaunchPage,
        incomingFile = incomingFile
    )
})

#----------------------------------------------------------------------
# return reactive with path to incoming file
#----------------------------------------------------------------------
list(
    file = incomingFile,
    sendFeedback = sendFeedback
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
