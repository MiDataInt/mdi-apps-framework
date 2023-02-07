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
    samples <- samples[, c('Project', 'Sample_ID', 'Description')]
    names <- app[[stepName]]$outcomes$sampleNames()
    x <- apply(samples, 1, function(v){     # get the user's assigned name, or description as a default
        key <- paste(v[1], v[2], sep = ":") # at this point, working with all known samples
        trimws(if(is.null(names[[key]])) v[3] else names[[key]])
    })
    if(makeUnique){ # prepend project if requested to try to make strings unique
        isDup <- duplicated(x) | rev(duplicated(rev(x)))
        x <- ifelse(isDup, paste(samples$Project, x, sep = ":"), x)
    }
    if(!is.null(sampleIds)) { # filter for specific samples, maintaining query order
        y <- sapply(sampleIds, function(sampleId){
            i <- which(samples$Sample_ID == sampleId)
            switch(as.character(length(i)), "0" = "unknown sampleId", "1" = x[i], "ambiguous sampleId")
        }) 
        names(y) <-  sampleIds
        y
    } else if(!is.null(sampleUniqueIds)) {
        sUIds <- getSampleUniqueIds()
        y <- sapply(sampleUniqueIds, function(sampleUniqueId){
            i <- which(sUIds == sampleUniqueId)
            switch(as.character(length(i)), "0" = "unknown sampleUniqueId", "1" = x[i], "ambiguous sampleUniqueId")
        }) 
        names(y) <-  sampleUniqueIds
        y
    }  else x[rows]
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
    sources[[sourceId]]
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

# check that a set of incoming sourceIds are still found in mdi/data/packages
# input must match that of sourceFileUpload$outcomes$sources
# where list names are sourceIds, and each source has a fileName key:value pair
checkBookmarkPackageExistence <- function(sources){
    present <- sapply(names(sources), function(sourceId){
        dir <- getPackageDir(sourceId)
        dir.exists(dir) && file.exists(file.path(dir, "package.yml"))
    })
    if(all(present)) return(TRUE)
    missing <- sapply(sources[!present], function(x) x$fileName)
    showUserDialog(
        "Missing Data Packages", 
        tags$p(
            "The following data packages were not found on this MDI server."
        ), 
        tags$ul(
            lapply(missing, tags$li)
        ),
        tags$p(
            "Either this bookmark was constructed on another server or ",
            "the prior data package uploads were subsequently deleted."
        ),
        tags$p(
            "You need to (re)upload each missing data package to (re)activate this bookmark on this server."
        ),
        callback = function(parentInput) NULL,
        size = "m", 
        type = 'okOnly', 
        footer = NULL, 
        easyClose = FALSE, 
        fade = TRUE
    )
    FALSE
}
