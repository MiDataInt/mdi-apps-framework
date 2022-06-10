#----------------------------------------------------------------------
# static components for sample source file upload
# handles all types including manifests, package zips, bookmarks and data tables
# used on launch page and in sourceFileUpload module
#----------------------------------------------------------------------

# module ui function
sourceFileInputUI <- function(id, appName=NULL, externalSuffixes=c(), width='100%') {
    
    # initialize namespace
    ns <- NS(id)

    # as needed, enable the server-side file browser ...
    if(exposeServerFiles()){
        localWidth <- 9        
        fileServerButton <- column(
            width = 12 - localWidth, 
            serverSourceFilesButtonUI( ns('serverFileInput') )
        )
    } else {
        localWidth <- 12        
        fileServerButton <- ""
    }

    # ... and always the local file upload input
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
                    accept = getAllowedSourceFileTypes(appName, externalSuffixes, extensionOnly = TRUE),
                    width = width
                )
            ),
            column(
                width = 12, 
                uiOutput(ns('fileInputFeedback'))
            )
        )
    )                       
}
