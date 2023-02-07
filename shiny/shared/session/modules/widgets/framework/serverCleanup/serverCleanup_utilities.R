#----------------------------------------------------------------------
# utilities for populating a dialog with an interface to delete dataDir folders
#----------------------------------------------------------------------
serverCleanupLink <- function(id, class = NULL, isHeader = TRUE){
    if(is.null(class) && isHeader) class <- "header-link"
    actionLink(id, NULL, icon = icon("broom"), class = class)
}

#----------------------------------------------------------------------
# launch an interface to delete dataDir folders
#----------------------------------------------------------------------
showServerCleanup <- function(
    session
){
    id <- "serverCleanupDialog"
    nsId <- session$ns(id)
    ns <- NS(nsId)
    onExit <- function(...){
        removeMatchingInputValues(session, id)
        dataPackagesCache <<- destroyModuleObservers(dataPackagesCache)   
    }
    dataPackagesCache <<- serverCleanupServer(id)
    showUserDialog(
        HTML(paste(
            "Clean Up Data Directory", 
            tags$i(
                id = "serverCleanupSpinner",
                class = "fas fa-spinner fa-spin",
                style = "margin-left: 2em; color: #3c8dbc; display: none;"
            )
        )), 
        serverCleanupUI(nsId),
        size = "l", 
        type = 'dismissOnly', 
        easyClose = FALSE,
        fade = FALSE,
        callback = onExit
    )
}

#----------------------------------------------------------------------
# support utilities
#----------------------------------------------------------------------
scanPackageDir <- function(packageDir){
    packageYmlFile <- file.path(packageDir, "package.yml")
    if(!file.exists(packageYmlFile)) return(NULL)
    package <- read_yaml(packageYmlFile)
    data.table(
        packageDir = packageDir,
        packageYmlFile = packageYmlFile,
        sourceId = basename(packageDir),
        pipelineShort = package$pipeline,
        pipelineLong = strsplit(package$task$pipeline, ":")[[1]][1],
        action = package$action,
        dataName = package$task[[package$action]]$output[["data-name"]]
    )
}
getPackageLevelDirs <- function(dir){
    dirs <- list.dirs(dir, recursive = FALSE, full.names = TRUE)
    if(length(dirs) == 0) return(dirs)
    dirs[nchar(basename(dirs)) == 2]
}
packagesAreScanned <- function(){
    !(is.null(dataPackagesCache) ||
      !is.list(dataPackagesCache) ||
      length(dataPackagesCache) == 0 || 
      is.null(dataPackagesCache$dataPackages))
}
scanDataPackages <- function(session){
    if(packagesAreScanned()) return(dataPackagesCache$dataPackages)
    startSpinner(session, message = "scanning data directory")    
    level1Dirs <- getPackageLevelDirs(file.path(serverEnv$DATA_DIR, "packages"))
    req(level1Dirs)
    dt <- do.call(rbind, lapply(level1Dirs, function(level1Dir) {
        do.call(rbind, lapply(getPackageLevelDirs(level1Dir), function(level2Dir) {
            packageDirs <- list.dirs(level2Dir, recursive = FALSE, full.names = TRUE)
            do.call(rbind, lapply(packageDirs, scanPackageDir))
        }))
    }))
    req(dt)   
    dt[, replicates := .N, by = list(pipelineLong, action, dataName)] 
    dt <- cbind(dt, mtime = file.info(dt$packageYmlFile, extra_cols = FALSE)$mtime)
    stopSpinner(session)
    dt[order(pipelineLong, action, dataName, mtime)]
}
