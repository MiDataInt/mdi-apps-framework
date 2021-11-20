#----------------------------------------------------------------
# source data file upload functions
#----------------------------------------------------------------

# get the list of allowed source file types for an app
getAllowedSourceFileTypes <- function(appName = NULL, externalSuffixes = list()){
    fs <- CONSTANTS$fileSuffixes
    
    # first source file load from the launch page
    # bookmark files only allowed here
    if(is.null(appName)){ 
        list(manifest  = fs$manifest,
             project   = fs$project,
             dataTable = fs$dataTable,             
             bookmark  = fs$bookmark)
        
    # a chance to upload additional manifest files to submit to a Stage 1 pipeline
    } else if (appName == CONSTANTS$apps$pipelineRunner){
        list(manifest  = fs$manifest)
    
    # a chance to upload additional data source files for a running Stage 2 app
    } else { 
        list(project   = fs$project,
             dataTable = fs$dataTable,
             external  = externalSuffixes)
    }
}

# check to see if an incoming file matches an allowed type
isAllowedSourceFileType <- function(file, allowedFileTypes){
    any(endsWith(file, unlist(allowedFileTypes)))
}

# get the type of an incoming file
getIncomingFileType <- function(fileName){
    fs  <- CONSTANTS$fileSuffixes
    sft <- CONSTANTS$sourceFileTypes
    if(endsWith(fileName, fs$bookmark))
        sft$bookmark
    else if(endsWith(fileName, fs$project))
        sft$project
    else if(endsWith(fileName, fs$manifest) &&
            grepl('DEMUX', toupper(fileName)))
        sft$manifest
    else if(endsWith(fileName, fs$dataTable))
        sft$dataTable
    else NULL  
}

# read a project file manifest, i.e. package.yml
getProjectFileConfig <- function(projectFile, sendFeedback){
    tryCatch({
        ymlFile <- unzip(projectFile, files = "package.yml", exdir = sessionDirectory)    
        config <- read_yaml(ymlFile)
        unlink(ymlFile)
        config        
    }, error = function(e) sendFeedback("improper or corrupt AGC project package", isError = TRUE))
}

# get all possible target apps for a given project file
# might be more than one app for a more generic project data type
getTargetAppsFromProjectFile <- function(projectFile, sendFeedback){
    uploadType <- getProjectFileConfig(projectFile, sendFeedback)$uploadType
    if(is.null(uploadType)) sendFeedback("improper or corrupt AGC project package", isError = TRUE)
    apps <- appUploadTypes[[uploadType]]
    if(is.null(apps) || length(apps) == 0){
        sendFeedback(paste('upload type', uploadType, 'is not supported by any current apps'), isError = TRUE) 
    }
    apps
}

# get exactly one target app for a project file
# query user if more than one is possible
getTargetAppFromProjectFile <- function(projectFile, sendFeedback){
    apps <- getTargetAppsFromProjectFile(projectFile, sendFeedback)
    if(length(apps) == 1) return(apps)
    sendFeedback("PENDING: query user for app selection when multiple possibilities", isError = TRUE) 
}

#----------------------------------------------------------------------
# handlers for the different types of incoming files
# perform initial validation and pass to app or initial page launch
#----------------------------------------------------------------------
loadIncomingFile <- function(file, allowedFileTypes, sendFeedback,
                             isLaunchPage=TRUE, incomingFile=NULL){
    reportProgress('loadIncomingFile')

    # check for valid work to do
    type <- getIncomingFileType(file$name)
    if(is.null(type) || !isAllowedSourceFileType(file$name, allowedFileTypes)){
        sendFeedback('unknown or unsupported file type', isError = TRUE)
    }
    
    # initialize common values and actions
    sft <- CONSTANTS$sourceFileTypes
    launchApp <- function(type, appName){
        loadRequest(list(app = appName, file = list(name = file$name, path = file$datapath,
                                                type = type, nocache = file$nocache)))
    }
    addDataSource <- function(type){
        incomingFile(list(name = file$name, path = file$datapath, type = type))
    }    

    # bookmark files (only allowed from launch page, always loads target app)
    if(type == sft$bookmark){
        appName <- getTargetAppFromBookmarkFile(file$datapath, sendFeedback)$app
        launchApp(type, appName)
    
    # project package files; the output from a magc-portal-pipeline
    } else if(type == sft$project){
        if(isLaunchPage){ # first file upload on launch page
            appName <- getTargetAppFromProjectFile(file$datapath, sendFeedback)
            if(is.null(appName)) return()
            launchApp(type, appName)
        } else { # additional file upload from within an app
            apps <- getTargetAppsFromProjectFile(file$datapath, sendFeedback)
            if(!(app$NAME %in% apps)) {
                error <- paste('uploaded project file is not compatible with the', app$NAME, 'app')
                sendFeedback(error, isError = TRUE)
            }
            addDataSource(type)
        }
 
    # sample manifests destined for execution by a magc-portal-pipeline 
    } else if(type == sft$manifest){
        if(isLaunchPage){ # first file upload on launch page
            launchApp(type, CONSTANTS$apps$pipelineRunner)
        } else { # additional file upload from within an app
            addDataSource(type)
        }

    # user-provided external data table in csv format
    # exact supported format varies with app
    } else if(type == sft$dataTable){
        if(isLaunchPage){ # first file upload on launch page
            appName <- rev(strsplit(file$name, '\\.')[[1]])[2]
            fileUsage <- ": expected pattern = '*.<app-name>.csv'"
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
        sendFeedback('code error: report to AGC', isError = TRUE)
    }
    sendFeedback(NULL)  
}
