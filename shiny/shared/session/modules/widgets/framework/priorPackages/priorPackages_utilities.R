#----------------------------------------------------------------------
# utilities for creating a button to load a previously loaded data package
#----------------------------------------------------------------------
priorPackagesButtonUI <- function(id){
    actionButton(id, "Prior Package", width = "100%")
}

#----------------------------------------------------------------------
# launch an interface to delete dataDir folders
#----------------------------------------------------------------------
showPriorPackages <- function(
    session,
    sendFeedback
){
    id <- "priorPackagesDialog"
    nsId <- session$ns(id)
    ns <- NS(nsId)
    onExit <- function(...){
        removeMatchingInputValues(session, id)
        dataPackagesCache <<- destroyModuleObservers(dataPackagesCache)
        launchApp <- function(appToLoad, file){
            loadRequest(list(
                app = appToLoad,
                file = file,
                suppressUnlink = TRUE
            ))                
        }
        if(isTruthy(dataPackagesCache$selectedRow)){
            package <- dataPackagesCache$dataPackages[dataPackagesCache$selectedRow]
            file <- list(
                type = "priorPackage",
                path = package$packageYmlFile,
                name = package[, paste(dataName, pipelineShort, action, "mdi.package.zip", sep = ".")],
                sourceId = package$sourceId
            )
            if(is.null(app$NAME) || app$NAME == "launch-page"){
                getTargetAppFromPackageYmlFile(package$packageYmlFile, sendFeedback, launchApp, file)               
            } else {
                firstStep <- app[[ names(app$config$appSteps)[1] ]]
                firstStep$loadSourceFile(file, suppressUnlink = TRUE)               
            }
        }
    }
    dataPackagesCache <<- priorPackagesServer(id)
    showUserDialog(
        "Select Prior Data Package", 
        priorPackagesUI(nsId),
        size = "l", 
        type = 'okCancel', 
        easyClose = FALSE,
        fade = FALSE,
        callback = function(...) setTimeout(onExit)
    )
}
