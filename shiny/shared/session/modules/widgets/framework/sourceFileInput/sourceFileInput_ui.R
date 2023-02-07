#----------------------------------------------------------------------
# static components for sample source file upload
# handles all types including manifests, package zips, bookmarks and data tables
# used on launch page and in sourceFileUpload module
#----------------------------------------------------------------------

# module ui function
sourceFileInputUI <- function(
    id, 
    appName=NULL, 
    externalSuffixes=c(), 
    width='100%', 
    createButtonUI = NULL,
    priorPackages = TRUE
) {
    
    # initialize namespace
    ns <- NS(id)

    # as needed, enable the Create New button ...
    localWidth <- 12
    buttonWidth <- 3
    createNewButton <- if(!is.null(createButtonUI)){
        localWidth <- localWidth - buttonWidth
        column(
            width = buttonWidth, 
            createButtonUI(ns('createNew'), width = "100%")
        )
    } else ""

    # ... and the server-side file browser ...
    fileServerButton <- if(exposeServerFiles()){
        localWidth <- localWidth - buttonWidth
        column(
            width = buttonWidth, 
            serverSourceFilesButtonUI(ns('serverFileInput'))
        )
    } else ""

    # ... and access to previously loaded data packages ...
    priorPackagesButton <- if(priorPackages){ # TODO: implement access security
        localWidth <- localWidth - buttonWidth
        column(
            width = buttonWidth, 
            priorPackagesButtonUI(ns('loadPriorDataPackage'))
        )
    } else ""

    # ... and always the local file upload input
    tags$div(
        fluidRow(
            class = "file-input-controls",  
            createNewButton,  
            fileServerButton,
            priorPackagesButton,
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
