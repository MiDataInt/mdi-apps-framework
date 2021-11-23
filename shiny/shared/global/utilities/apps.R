#----------------------------------------------------------------
# discover and characterize all apps in all installed Stage 2 suites
# developer-forks override definitive if present and in developer mode
#----------------------------------------------------------------

# the working Stage 2 apps suites, including pipelineRunner in apps framework
getAppSuiteDirs <- function(){
    getForkSuiteDirs <- function(fork) {
        dirs <- list.dirs(file.path(serverEnv$SUITES_DIR, fork), recursive = FALSE, full.names = TRUE)
        isAppsSuite <- sapply(dirs, function(dir) dir.exists(file.path(dir, 'shiny', 'apps')))
        c(dirs[isAppsSuite], file.path(serverEnv$FRAMEWORKS, fork, 'mdi-apps-framework'))
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
    appDirs
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

# convert an app directory into various useful bits and paths
parseAppDirectory <- function(appDir){
    appDir <- strsplit(appDir, '/')[[1]]
    dirApp <- rev(appDir)
    suiteDir <- paste(rev(dirApp[4:length(dirApp)]), collapse = '/')
    suiteSharedDir  <- file.path(suiteDir, 'shiny', 'shared')
    suiteGlobalDir  <- file.path(suiteSharedDir, 'global')
    suiteSessionDir <- file.path(suiteSharedDir, 'session')
    list(
        name  = dirApp[1],
        suite = dirApp[4],
        fork  = dirApp[5],
        suiteDir = suiteDir,
        suiteSharedDir = suiteSharedDir,
        suiteGlobalDir = suiteGlobalDir,
        suiteSessionDir = suiteSessionDir,
        suiteSharedClassesDir = file.path(suiteGlobalDir, 'classes'),
        suiteSharedModulesDir = file.path(suiteSessionDir, 'modules'),
        suiteSharedTypesDir   = file.path(suiteSessionDir, 'types')
    )
}
