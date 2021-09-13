
#----------------------------------------------------------------------
# static components for sample source file upload
# handles all types including manifests, project zips, bookmarks and data tables
# used on both launch page and sourceFileUpload module
#----------------------------------------------------------------------

# module ui function
sourceFileInputUI <- function(id, appName=NULL, externalSuffixes=c(), width='100%') {
    
    # initialize namespace
    ns <- NS(id)

    # file upload input
    tags$div(
        fluidRow(
            class = "file-input-controls",
            if(serverEnv$IS_GLOBUS) column(width=3, getGlobusFileButtonUI(ns('globusFileImport'), 'Select File on Globus')) else NULL,               
            column(width=if(serverEnv$IS_GLOBUS) 9 else 12, fileInput(
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

