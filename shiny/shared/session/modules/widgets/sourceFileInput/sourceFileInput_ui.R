#----------------------------------------------------------------------
# static components for sample source file upload
# handles all types including manifests, project zips, bookmarks and data tables
# used on launch page and in sourceFileUpload module
#----------------------------------------------------------------------

# module ui function
sourceFileInputUI <- function(id, appName=NULL, externalSuffixes=c(), width='100%') {
    
    # initialize namespace
    ns <- NS(id)

    # file upload input
    tags$div(
        fluidRow(
            class = "file-input-controls",           
            column(width = 12, fileInput(
                ns('fileInput'),
                NULL,
                multiple = FALSE,
                accept = getAllowedSourceFileTypes(appName, externalSuffixes),
                width = width
            ))
         
        ),
        uiOutput(ns('fileInputFeedback'))
    )                       
}
