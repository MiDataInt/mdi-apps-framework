#----------------------------------------------------------------------
# resolve standardized data file paths
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize data output directories
#----------------------------------------------------------------------
initializeAppDataPaths <- function(){
    dataDirs$packages <<- file.path(serverEnv$DATA_DIR, 'packages') # input data from Stage 1 pipeline packages
    dataDirs$analyses <<- file.path(serverEnv$DATA_DIR, 'analyses') # output data from Stage 2 app analyses
    for(path in dataDirs) dir.create(path, showWarnings = FALSE)
}      
        
#----------------------------------------------------------------------
# create nested, keyed directories for faster file retrieval
# obviously, for on-disk databasing, not intended to be human readable
#----------------------------------------------------------------------
# format = parentDir/XX/YY/XXYY...
# e.g. parentDir/ec/2d/ec2df02ef10299cbdcc9a45d497cffa1
#----------------------------------------------------------------------
getKeyedDir <- function(parentDir, id, create=FALSE){
    if(!dir.exists(parentDir)) dir.create(parentDir)
    dirs <- regmatches(id, gregexpr('..', id))[[1]]
    dir1 <- file.path(parentDir, dirs[1])
    if(!dir.exists(dir1)) dir.create(dir1)
    dir2 <- file.path(dir1, dirs[2])
    if(!dir.exists(dir2)) dir.create(dir2)
    dir <- file.path(dir2, id)
    if(create && !dir.exists(dir)) dir.create(dir)
    dir
}

#----------------------------------------------------------------------
# input data files from a package zip
#----------------------------------------------------------------------
# these are nearly always processed files that are:
#   the output of a prior Stage 1 pipeline
#   the input to Stage 2 apps
#----------------------------------------------------------------------
getPackageDir <- function(packageId){
    getKeyedDir(dataDirs$packages, packageId)
}
getPackageFileByName <- function(packageId, filename){
    file.path(getPackageDir(packageId), filename)
}
getPackageFileByType <- function(package, type){
    file <- package$config$files[[type]]
    if(is.null(file)) return(NULL)
    name <- file$file
    list(
        name = name,
        path = file.path(package$dataDir, name)
    )
}

#----------------------------------------------------------------------
# output files from an analysis job
#----------------------------------------------------------------------
getAnalysisDir <- function(schemaId, create=FALSE) {
    getKeyedDir(dataDirs$analyses, schemaId, create = create)
}
getOutputFile <- function(schemaId, filename, create=FALSE) {
    file.path(getAnalysisDir(schemaId, create), filename)
}
getJobStatusFile <- function(schemaId, create=FALSE){
    getOutputFile(schemaId, 'status.txt', create)
}
getJobRDataFile  <- function(schemaId, create=FALSE){
    getOutputFile(schemaId, 'results.RData', create)
}
getJobRdsFile  <- function(schemaId, create=FALSE){
    getOutputFile(schemaId, 'results.rds', create)
}

#----------------------------------------------------------------------
# remove output files from disk upon analysis schema delete
#   action was already confirmed with user upstream
#----------------------------------------------------------------------
purgeOutputFiles <- function(schemaId){
    unlink(getAnalysisDir(schemaId), recursive = TRUE, force = FALSE) 
}

#getProjectFileByParentType <- function(manifest, parentType, type, fileN=TRUE){
#    if(is.null(fileN)) fileN <- TRUE
#    name <- manifest$contentFiles[[parentType]][[type]][fileN]
#    list(
#        name = name,
#        path = file.path(manifest$dataDir, name)
#    )
#}
#getProjectFileByContentType <- function(manifest, type, fileN=TRUE){
#    getProjectFileByParentType(manifest, 'byContentType', type, fileN)
#}
#getProjectFileByFileType <- function(manifest, type, fileN=TRUE){
#    getProjectFileByParentType(manifest, 'byFileType',    type, fileN)
#}
#getProjectFile <- function(options){
#    if(!is.null(options$filename)){ # exactly named files
#        filePathsToList(
#            getProjectFileByName(options$manifestId, options$filename)
#        , options$fileN)
#    } else if(!is.null(options$pattern)){ # by regex pattern matching
#        filePathsToList(
#            list.files(getManifestDir(options$manifestId), options$pattern, full.names=TRUE)
#        , options$fileN)
#    } else if(!is.null(options$contentType)){ # by content type
#        manifest <- getManifestFromId(options$manifestId)
#        getProjectFileByContentType(manifest, options$contentType, options$fileN) 
#    } else if(!is.null(options$fileType)){ # by file type
#        manifest <- getManifestFromId(options$manifestId)
#        getProjectFileByFileType(   manifest, options$fileType,    options$fileN) 
#    } else { # bad request
#        NULL
#    }
#}
#filePathsToList <- function(paths, fileN=TRUE){
#    if(is.null(fileN)) fileN <- TRUE
#    paths <- paths[fileN]
#    list(
#        name = sapply(strsplit(paths, '/'), function(v) v[length(v)]),
#        path = paths
#    )
#}
