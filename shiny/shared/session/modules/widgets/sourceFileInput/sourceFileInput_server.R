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

#----------------------------------------------------------------------
# enable the local file upload input
#----------------------------------------------------------------------
handleIncomingSourceFile <- function(file){
    reportProgress('handleIncomingSourceFile')
    req(file)
    req(is.list(file))

    str(file)

#     app-server_1              | 'data.frame':       1 obs. of  4 variables:
# app-server_1              |  $ name    : chr "GarySmith-2299-LK.mdi.package.zip"
# app-server_1              |  $ size    : int 1503824
# app-server_1              |  $ type    : chr "application/x-zip-compressed"
# app-server_1              |  $ datapath: chr "/tmp/RtmpYlwlAf/f16a85b3f9d45278b703288b/0.zip"

# app-server_1              |  $ files:List of 1
# app-server_1              |   ..$ 0:List of 4
# app-server_1              |   .. ..$ : chr ""
# app-server_1              |   .. ..$ : chr "human_embryo"
# app-server_1              |   .. ..$ : chr "2299-LK"
# app-server_1              |   .. ..$ : chr "GarySmith-2299-LK.mdi.package.zip"
# app-server_1              |  $ root : chr "Projects"


    loadIncomingFile(
        file = file,
        allowedFileTypes = allowedFileTypes,
        sendFeedback = sendFeedback,
        isLaunchPage = isLaunchPage,
        incomingFile = incomingFile
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
if(serverEnv$IS_SERVER && isAuthorizedUser()) {
    serverFilesButtonServer(
        'serverFileInput', 
        input, 
        session, 
        rw = "read", 
        filetypes = c("mdi", "zip", "csv"),
        loadFn = handleIncomingSourceFile
    ) 
}

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
