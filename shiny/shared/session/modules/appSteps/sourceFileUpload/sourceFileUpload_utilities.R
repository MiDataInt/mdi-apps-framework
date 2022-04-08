#----------------------------------------------------------------------
# retrieve sample names and unique identifiers
#----------------------------------------------------------------------
# depends on sourceFileUpload or comparable replacement module
# which must set app[[stepName]]$outcomes$samples and $sampleNames reactives
#----------------------------------------------------------------------
# names can be overriden by user edits and are stored in keyed list
# sample unique Ids = Project:Sample_ID
#----------------------------------------------------------------------

# get the working sample names, either automated or overridden by user
# ensure that these are unique by adding project/run only as needed
getSampleNames <- function(rows = TRUE, sampleIds = NULL, sampleUniqueIds = NULL, makeUnique = FALSE){
    stepName <- appStepNamesByType$upload
    samples <- app[[stepName]]$outcomes$samples()
    if(!is.null(sampleIds)) rows <- samples$Sample_ID %in% sampleIds
    if(!is.null(sampleUniqueIds)) rows <- getSampleUniqueIds() %in% sampleUniqueIds
    samples <- samples[, c('Project', 'Sample_ID', 'Description')]
    names <- app[[stepName]]$outcomes$sampleNames()
    names <- apply(samples, 1, function(v){
        key <- paste(v[1], v[2], sep = ":")
        if(is.null(names[[key]])) v[3] else names[[key]]
    })
    x <- if(makeUnique){
        isDup <- duplicated(names) | rev(duplicated(rev(names)))
        ifelse(isDup, paste(samples$Project, names, sep = ":"), names)[rows]
    } else {
        names[rows]    
    }
    if(!is.null(sampleIds)) {
        names(x) <- samples[rows, 'Sample_ID'] # if samples requested by id, return a named vector
        x <- x[sampleIds]
    }
    trimws(x) # remove leading and trailing whitespace for accurate matching later on
}
getSampleName <- function(sample){ # sample is a one row of the samples() table we wish to match
    stepName <- appStepNamesByType$upload
    names <- app[[stepName]]$outcomes$sampleNames()
    apply(sample[, c('Project', 'Sample_ID', 'Description')], 1, function(v){
        key <- paste(v[1], v[2], sep = ":")
        if(is.null(names[[key]])) v[3] else names[[key]]
    })
}

# get the unique identifiers for all active samples
getSampleUniqueIds <- function(samples=NULL, rows=TRUE, sourceId=NULL){
    stepName <- appStepNamesByType$upload
    if(is.null(samples)) samples <- app[[stepName]]$outcomes$samples()
    samples <- samples[rows, c('Source_ID', 'Project', 'Sample_ID')]
    if(!is.null(sourceId)) samples <- samples[samples$Source_ID == sourceId, ]
    apply(samples[, c('Project', 'Sample_ID')], 1, paste, collapse = ":")
}

# get the full source entry from its ID
getSourceFromId <- function(sourceId){
    stepName <- appStepNamesByType$upload
    sources <- if(isParentProcess) app[[stepName]]$outcomes$sources()
                 else jobParameters$sources # workers must have converted reactive to static
    sources[[ names(sources) == sourceId ]]
}

# get a file from a source by type or name
getSourceFile <- function(source, fileType){ # just the file name
    if(is.null(source) || is.null(fileType)) return(NULL)
    source$config$files[[fileType]]
}
getSourceFilePath <- function(sourceId, fileType, parentDir=NULL){ # when we know a file by type
    if(is.null(parentDir)) parentDir <- file.path(serverEnv$DATA_DIR, 'packages')
    source <- getSourceFromId(sourceId)
    dir <- getKeyedDir(parentDir, sourceId)
    file <- getSourceFile(source, fileType)
    file.path(dir, file$file)
}
expandSourceFilePath <- function(sourceId, fileName, parentDir=NULL){ # when we know a file by name
    if(is.null(parentDir)) parentDir <- file.path(serverEnv$DATA_DIR, 'packages')
    dir <- getKeyedDir(parentDir, sourceId)
    file.path(dir, fileName)
}
getSourceFilePackageName <- Vectorize(function(sourceId){
    source <- getSourceFromId(sourceId)
    source$unique$Project[1]
})

# get information about a data package from its source ID
getSourcePackageOption <- function(sourceId, optionFamily, option){
    source <- getSourceFromId(sourceId)
    req(source)
    req(source$sourceType == "package")
    action <- source$config$action
    options <- source$config$task[[action]]
    req(options[[optionFamily]])
    options[[optionFamily]][[option]]
}
