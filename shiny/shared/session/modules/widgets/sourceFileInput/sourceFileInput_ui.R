#----------------------------------------------------------------------
# static components for sample source file upload
# handles all types including manifests, package zips, bookmarks and data tables
# used on launch page and in sourceFileUpload module
#----------------------------------------------------------------------

# module ui function
sourceFileInputUI <- function(id, appName=NULL, externalSuffixes=c(), width='100%') {
    
    # initialize namespace
    ns <- NS(id)

# as needed, enable the server-side file browser
    if(serverEnv$IS_SERVER && isAuthorizedUser()){
        localWidth <- 9        
        fileServerButton <- column(
            width = 12 - localWidth, 
            serverFilesButtonUI( ns('serverFileInput') )
        )
    } else {
        localWidth <- 12        
        fileServerButton <- ""
    }


    # file upload input
    tags$div(
        fluidRow(
            class = "file-input-controls",    
            fileServerButton,
            column(
                width = localWidth, 
                fileInput(
                    ns('fileInput'),
                    NULL,
                    multiple = FALSE,
                    accept = getAllowedSourceFileTypes(appName, externalSuffixes),
                    width = width
                )
            )
        ),
        uiOutput(ns('fileInputFeedback'))
    )                       
}
