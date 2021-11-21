#----------------------------------------------------------------
# discover and characterize all apps in all installed Stage 2 suites
# developer-forks override definitive if present and in developer mode
#----------------------------------------------------------------

# the working Stage 2 apps suites, including pipelineRunner in apps framework
getAppSuiteDirs <- function(){
    getForkSuiteDirs <- function(fork) {
        dirs <- list.dirs(file.path(serverEnv$SUITES_DIR, fork), recursive = FALSE, full.names = TRUE)
        isAppsSuite <- sapply(dirs, function(dir) dir.exists(file.path(dir, 'shiny', 'apps')))
        c(dirs[isAppsSuite], file.path(serverEnv$FRAMEWORKS, fork))
    }
    list(
        definitive = getForkSuiteDirs("definitive"),
        developer = if(serverEnv$IS_DEVELOPER) getForkSuiteDirs("developer-forks") else character()
    )
}

# paths to working app directories
getAppDirs <- function(appSuiteDirs){ 
    appDirs <- list()
    for(appSuiteDir in c(appSuiteDirs$definitive, appSuiteDirs$developer)){
        appsDir <- file.path(appSuiteDir, 'shiny', 'apps')
        appNames <- list.dirs(appsDir, recursive = FALSE, full.names = FALSE)
        for(appName in appNames) appDirs[[appName]] <- file.path(appsDir, appName)
    }
    appsDirs
}

# upload types accepted by each app, stratified by upload type
getAppUploadTypes <- function(appDirs){
    uploadTypes <- list()
    for(appDir in appDirs){
        appName <- rev(strsplit(appDir, '/')[[1]])[1]
        x <- read_yaml(file.path(appDir, 'config.yml'))$uploadTypes
        if(is.null(x)) next
        for(uploadType in names(x)){
            if(is.null(uploadTypes[[uploadType]])){
                uploadTypes[[uploadType]] <- appName
            } else {
                uploadTypes[[uploadType]] <- c(uploadTypes[[uploadType]], appName)
            }
        }
    }
    uploadTypes
}
