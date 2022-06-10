#----------------------------------------------------------------
# source data file upload functions
#----------------------------------------------------------------

# determine whether to show the shinyFile server file access button
exposeServerFiles <- function(){
    if(serverEnv$IS_LOCAL && serverEnv$DEBUG) TRUE
    else if(serverEnv$REQUIRES_AUTHENTICATION) isAuthorizedUser()
    else !serverEnv$IS_LOCAL # but do use shinyFiles in remote and ondemand modes
}

# get the list of allowed incoming file types for a given app
getAllowedSourceFileTypes <- function(
    appName = NULL, 
    externalSuffixes = list(),
    extensionOnly = FALSE # if true, only return ".zip", not ".xxx.zip", etc.
){
    fs <- CONSTANTS$fileSuffixes

    # first source file load from the launch page
    # bookmark files only allowed here
    suffixes <- if(is.null(appName)){ 
        x <- list(
            jobFile   = fs$jobFile,
            # manifest  = fs$manifest, # DEPRECATED
            package   = fs$package,
            dataTable = fs$dataTable,             
            bookmark  = fs$bookmark,
            book      = fs$book
        )
        if(serverEnv$IS_WINDOWS) x$jobFile <- fs$jobFile
        x

    # a chance to upload additional manifest files to submit to a Stage 1 pipeline
    } else if (appName == CONSTANTS$apps$pipelineRunner){
        list(jobFile   = fs$jobFile) # manifest  = fs$manifest DEPRECATED
    
    # a chance to upload additional data source files for a running Stage 2 app
    } else { 
        list(
            package   = fs$package,
            dataTable = fs$dataTable,
            external  = externalSuffixes
        )
    }

    # parse the return value
    if(extensionOnly){
        suffixes <- strsplit(unlist(suffixes), "\\.")
        suffixes <- sapply(suffixes, function(x) x[length(x)])
        paste0(".", sort(unique(suffixes)))
    } else suffixes
}

# check to see if an incoming file matches an allowed type
isAllowedSourceFileType <- function(file, allowedFileTypes){
    any(endsWith(file, unlist(allowedFileTypes)))
}

# get the type of an incoming file
getIncomingFileType <- function(fileName){
    fs  <- CONSTANTS$fileSuffixes
    sft <- CONSTANTS$sourceFileTypes
    for(type in c('dataTable', 'manifest', 'jobFile', 'package', 'bookmark', 'book')){ # order is important 
        if(endsWith(fileName, fs[[type]])) return( sft[[type]] )
    }
    NULL
}

#----------------------------------------------------------------
# package file parsing and conversion to target app(s)
#----------------------------------------------------------------

# read a Stage 1 pipeline output package configuration, 
# i.e., the package.yml file in a xxx.mdi.package.zip
getPackageFileConfig <- function(packageFile, sendFeedback){
    tryCatch({
        ymlFile <- unzip(packageFile, files = "package.yml", exdir = sessionDirectory)    
        config <- read_yaml(ymlFile)
        unlink(ymlFile)
        config        
    }, error = function(e) {
        # print(e)
        sendFeedback("missing file 'package.yml' in pipeline package", isError = TRUE)
    })
}

# get all possible target apps for a given package file
# might be more than one app for a more generic package data type
getTargetAppsFromPackageFile <- function(packageFile, sendFeedback){
    uploadType <- getPackageFileConfig(packageFile, sendFeedback)$uploadType
    if(is.null(uploadType)) sendFeedback("missing tag 'uploadType' in pipeline package", isError = TRUE)
    apps <- appUploadTypes[[uploadType]]
    if(is.null(apps) || length(apps) == 0){
        sendFeedback(paste0("upload type '", uploadType, "' is not supported by any current apps"), isError = TRUE) 
    }
    apps
}

# get exactly one target app for a package file
# query user if more than one app is possible for a package
getTargetAppFromPackageFile <- function(packageFile, sendFeedback){
    apps <- getTargetAppsFromPackageFile(packageFile, sendFeedback)
    if(length(apps) == 1) return(apps)
    sendFeedback("PENDING: query user for app selection when multiple possibilities", isError = TRUE) 
}

#----------------------------------------------------------------------
# handlers for the different types of incoming files
# perform initial validation and pass to app or initial page launch
#----------------------------------------------------------------------
loadIncomingFile <- function(file, allowedFileTypes, sendFeedback,
                             isLaunchPage = TRUE, incomingFile = NULL,
                             suppressUnlink = NULL){
    reportProgress('loadIncomingFile')

    # check for valid work to do
    type <- getIncomingFileType(file$name)
    if(is.null(type) || !isAllowedSourceFileType(file$name, allowedFileTypes)){
        sendFeedback('unknown or unsupported file type', isError = TRUE)
    }
    
    # initialize common values and actions
    sft <- CONSTANTS$sourceFileTypes
    launchApp <- function(type, appName){
        nocache <- if('nocache' %in% names(file)) file$nocache else NULL # suppress shinyFiles tibble warning
        loadRequest(list(
            app = appName, 
            file = list(name = file$name, path = file$datapath,
                        type = type, nocache = nocache),
            suppressUnlink = suppressUnlink
        ))
    }
    addDataSource <- function(type){
        incomingFile(list(name = file$name, path = file$datapath, type = type, 
                          suppressUnlink = suppressUnlink))
    }    

    # bookmark files (only allowed from launch page, always loads target app)
    if(type == sft$bookmark || type == sft$book){
        appName <- getTargetAppFromBookmarkFile(file$datapath, sendFeedback)$app
        launchApp(type, appName)
    
    # pipeline package files; the output from a Stage 1 pipeline
    } else if(type == sft$package){
        if(isLaunchPage){ # first file upload on launch page
            appName <- getTargetAppFromPackageFile(file$datapath, sendFeedback)
            if(is.null(appName)) return()
            launchApp(type, appName)
        } else { # additional file upload from within an app
            apps <- getTargetAppsFromPackageFile(file$datapath, sendFeedback)
            if(!(app$NAME %in% apps)) {
                error <- paste('uploaded package file is not compatible with the', app$NAME, 'app')
                sendFeedback(error, isError = TRUE)
            }
            addDataSource(type)
        }
 
    # job configuration files (or sample manifests, deprecated) for execution by a Stage 1 pipeline 
    } else if(type == sft$jobFile || type == sft$manifest){
        if(isLaunchPage){ # first file upload on launch page
            launchApp(type, CONSTANTS$apps$pipelineRunner)
        } else { # additional file upload from within an app
            addDataSource(type)
        }

    # user-provided external data table in csv format
    # exact supported format varies with app
    } else if(type == sft$dataTable){
        if(isLaunchPage){ # first file upload on launch page
            appName <- rev(strsplit(file$name, '\\.')[[1]])[3]
            fileUsage <- ": expected pattern = '*.<app-name>.data.csv'"
            if(is.null(appName)) {
                error <- paste0('could not get app from file name', fileUsage)
                sendFeedback(error, isError = TRUE)
            }
            if(!(appName %in% names(appConfigs))) {
                error <- paste0('unknown app: ', appName, fileUsage)
                sendFeedback(error, isError = TRUE)
            }
            launchApp(type, appName)
        } else { # additional file upload from within an app; assume table is relevant to the app
            addDataSource(type)
        }
    
    # catch all
    } else {
        sendFeedback('code error: please create an issue on GitHub', isError = TRUE)
    }
    sendFeedback(NULL)  
}
