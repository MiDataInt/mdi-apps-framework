#----------------------------------------------------------------
# discover and characterize all installed Stage 1 pipelines and Stage 2 apps suites
# developer-forks override definitive if present and in developer mode
#----------------------------------------------------------------
suiteTargetPaths <- list(
    pipelines = 'pipelines',
    apps = file.path('shiny', 'apps')
)

#----------------------------------------------------------------
# generic
#----------------------------------------------------------------

# suite directories
getSuiteDirs <- function(targetPath, isApps = FALSE){
    getForkSuiteDirs <- function(fork) {
        dirs <- list.dirs(file.path(serverEnv$SUITES_DIR, fork), recursive = FALSE, full.names = TRUE)
        if(length(dirs) > 0){
            isTargetSuite <- sapply(dirs, function(dir) dir.exists(file.path(dir, targetPath)))
            dirs <- dirs[isTargetSuite]
        }
        if(isApps) dirs <- c(dirs, file.path(serverEnv$FRAMEWORKS, fork, 'mdi-apps-framework'))
        dirs
    }
    list(
        definitive = getForkSuiteDirs("definitive"),
        developer = if(serverEnv$IS_DEVELOPER) getForkSuiteDirs("developer-forks") else character()
    )
}

# paths to pipeline/app directories
getToolDirs <- function(suiteDirs, targetPath){ 
    toolDirs <- list()
    for(suiteDir in c(suiteDirs$definitive, suiteDirs$developer)){
        toolDir <- file.path(suiteDir, targetPath)
        toolNames <- list.dirs(toolDir, recursive = FALSE, full.names = FALSE)
        for(toolName in toolNames) toolDirs[[toolName]] <- file.path(toolDir, toolName)
    }
    toolDirs$`_template` <- NULL
    toolDirs
}

# convert a path to a tool suite and target name
parseToolDir <- function(toolDir, targetPath){
    delim <- paste0('/', targetPath, '/')
    parts <- rev(strsplit(toolDir, delim)[[1]])
    toolName <- parts[1]
    parts <- rev(strsplit(parts[2], '/')[[1]])
    list(
        name  = toolName,
        suite = parts[1],
        fork  = parts[2]
    )
}

# get a list of all installed suites of a type
getInstalledSuites <- function(toolDirs, targetPath){
    if(length(toolDirs) == 0) return(character())
    d <- lapply(toolDirs, function(x) parseToolDir(x, targetPath))
    suites <- sapply(d, function(x) x$suite)
    forks  <- sapply(d, function(x) x$fork)
    dirs <- file.path(serverEnv$SUITES_DIR, forks, suites)
    x <- dirs
    names(x) <- suites
    x[!duplicated(x)] # not unique, want to preserve the names
}

# get a list of tools available in a suite
getInstalledTools <- function(suiteDir, targetPath){
    x <- list.dirs(file.path(suiteDir, targetPath), recursive = FALSE, full.names = FALSE)
    x[x != '_template']
}

#----------------------------------------------------------------
# Stage 1 Pipeplines (for Pipeline Runner)
#----------------------------------------------------------------
getPipelineSuiteDirs <- function() getSuiteDirs(suiteTargetPaths$pipelines, isApps = FALSE)  
getPipelineDirs <- function(pipelineSuiteDirs) getToolDirs(pipelineSuiteDirs, suiteTargetPaths$pipelines)
parsePipelineSuite <- function(pipelineDir) parseToolDir(pipelineDir, suiteTargetPaths$pipelines)
getInstalledPipelineSuites <- function(pipelineDirs) {
    getInstalledSuites(pipelineDirs, suiteTargetPaths$pipelines)
}
getInstalledPipelines <- function(pipelineSuiteDir) getInstalledTools(pipelineSuiteDir, suiteTargetPaths$pipelines)

#----------------------------------------------------------------
# Stage 2 Apps
#----------------------------------------------------------------
getAppSuiteDirs <- function() getSuiteDirs(suiteTargetPaths$apps, isApps = TRUE)  
getAppDirs <- function(appSuiteDirs) getToolDirs(appSuiteDirs, suiteTargetPaths$apps)
parseAppSuite <- function(appDir) parseToolDir(appDir, suiteTargetPaths$apps)
getInstalledAppSuites <- function(appDirs) {
    getInstalledSuites(appDirs, suiteTargetPaths$apps)
}
getInstalledApps <- function(appSuiteDir) getInstalledTools(appSuiteDir, suiteTargetPaths$apps)

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
parseAppDirectory <- function(appDir, extended = FALSE){
    dirApp <- rev( strsplit(appDir, '/')[[1]] )
    suiteDir <- paste(rev(dirApp[4:length(dirApp)]), collapse = '/')
    suiteSharedDir  <- file.path(suiteDir, 'shiny', 'shared')
    suiteGlobalDir  <- file.path(suiteSharedDir, 'global')
    suiteSessionDir <- file.path(suiteSharedDir, 'session')
    extended <- if(extended){
        tryCatch({
            config <- read_yaml(file.path(appDir, "config.yml"))
            list(
                displayName = if(is.null(config$name)) dirApp[1] else config$name,
                description = if(is.null(config$description)) "not available" else config$description, 
                coldStartable = if(is.null(config$suppressColdStart)) TRUE else !config$suppressColdStart
            )            
        }, error = function(e) list(
            displayName = "",
            description = "config load error", 
            coldStartable = FALSE
        ))
    } else list(
        displayName = "",
        description = "", 
        coldStartable = NA
    )
    list(
        name  = dirApp[1],
        suite = dirApp[4],
        fork  = dirApp[5],
        appDir = appDir,
        appModulesDir = file.path(appDir, 'modules'),
        suiteDir = suiteDir,
        suiteSharedDir = suiteSharedDir,
        suiteGlobalDir = suiteGlobalDir,
        suiteSessionDir = suiteSessionDir,
        suiteSharedClassesDir = file.path(suiteGlobalDir, 'classes'),
        suiteSharedModulesDir = file.path(suiteSessionDir, 'modules'),
        suiteSharedTypesDir   = file.path(suiteSessionDir, 'types'),
        displayName   = extended$displayName,
        description   = extended$description,
        coldStartable = extended$coldStartable
    )
}

# parse external suite directories
getExternalSuiteDir <- function(suite){
    get_dir <- function(fork, allow = TRUE){
        if(!allow) return(NULL)
        dir <- file.path(serverEnv$SUITES_DIR, fork, suite)
        if(!dir.exists(dir)) return(NULL)
        dir
    }    
    suiteDir <- get_dir("developer-forks", serverEnv$IS_DEVELOPER)
    if(is.null(suiteDir)) suiteDir <- get_dir("definitive")
    suiteDir
}
parseExternalSuiteDirs <- function(suite){
    suiteDir <- getExternalSuiteDir(suite)
    if(is.null(suiteDir)) return(NULL)
    suiteSharedDir  <- file.path(suiteDir, 'shiny', 'shared')
    suiteGlobalDir  <- file.path(suiteSharedDir, 'global')
    suiteSessionDir <- file.path(suiteSharedDir, 'session')
    list(
        suiteDir = suiteDir,
        suiteSharedDir = suiteSharedDir,
        suiteGlobalDir = suiteGlobalDir,
        suiteSessionDir = suiteSessionDir,
        suiteSharedClassesDir   = file.path(suiteGlobalDir,  'classes'),
        suiteSharedModulesDir   = file.path(suiteSessionDir, 'modules'),
        suiteSharedTypesDir     = file.path(suiteSessionDir, 'types'),
        suiteSharedUtilitiesDir = file.path(suiteSessionDir, 'utilities')
    )
}

# get the declared dependencies of a given suite
getSuiteConfig <- function(suiteDir){
    suiteConfigFile <- file.path(suiteDir, "_config.yml")
    if(!file.exists(suiteConfigFile)) return(list())
    read_yaml(suiteConfigFile)
}
getSuiteDependencies <- function(suiteDir){
    suiteConfig <- getSuiteConfig(suiteDir)
    requiredSuites <- suiteConfig$suite_dependencies
    if(is.null(requiredSuites)) requiredSuites <- character()
    optionalSuites <- suiteConfig$optional_suite_dependencies
    if(is.null(optionalSuites)) optionalSuites <- character()
    optionalSuites <- optionalSuites[!(optionalSuites %in% requiredSuites)]
    allSuites <- c(optionalSuites, requiredSuites) # optional first in install list, so has lowest precedence
    if(length(allSuites) == 0) return(list())
    suiteVersions <- app$config$suiteVersions
    if(is.null(suiteVersions)) suiteVersions <- list()
    x <- lapply(unique(allSuites), function(gitRepo){
        repo <- strsplit(gitRepo, '/')[[1]]
        suite <- repo[2]
        list(
            gitRepo  = gitRepo,
            name     = suite,
            dir      = getExternalSuiteDir(suite),
            version  = suiteVersions[[suite]],             
            versions = NA, # filled after version is checked out by executeLoadRequest()
            head     = NA,
            optional = !(gitRepo %in% requiredSuites)
        )
    })
    names(x) <- sapply(x, function(xx) xx$name)
    x
}
