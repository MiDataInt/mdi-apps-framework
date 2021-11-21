#----------------------------------------------------------------------
# retrieve sample set names and unique identifiers
#----------------------------------------------------------------------
# depends on assignSamples or comparable replacement module
# which must set app[[stepName]]$sampleSets and $sampleSetNames reactives
#----------------------------------------------------------------------
# names can be overriden by user edits and are stored in keyed list
#----------------------------------------------------------------------

# get a single sample set name, with overrides
getSampleSetName <- function(id){
    stepName <- appStepNamesByType$assign
    name <- app[[stepName]]$outcomes$sampleSetNames()[[id]]
    if(is.null(name)) app[[stepName]]$outcomes$sampleSets()[[id]]$name # systematic
    else name # as assigned by user
}

# get the list of sample set names with user overrides
getSampleSetNames <- function(rows=TRUE){
    stepName <- appStepNamesByType$assign
    sapply(names(app[[stepName]]$outcomes$sampleSets())[rows], getSampleSetName)
}

# retrieve sample set information
getSampleSetsNamedList <- function(rows=TRUE){
    stepName <- appStepNamesByType$assign
    d <- names(app[[stepName]]$outcomes$sampleSets())[rows] # values = UIDs
    names(d) <- getSampleSetNames(rows) # names = human readable
    d
}

# retrieve the names of the assignSamples categories in use
# returns a list whose names are Category1... and values are the user-friendly names
# unless invert=TRUE, then names are the user-friendly names and values are Category1...
getCategoryNames <- function(plural=FALSE, invert=FALSE){
    stepName <- appStepNamesByType$assign
    categories <- app$config$appSteps[[stepName]]$options$categories
    if(is.null(categories) || length(categories) == 0) return(list())    
    tense <- if(plural) 'plural' else 'singular'    
    friendlyNames <- sapply(categories, function(x) x[[tense]])     
    categoryN     <- sapply(seq_along(categories), function(i) paste0('Category', i) )
    if(invert){
        x <- as.list(categoryN)
        names(x) <- friendlyNames
    } else {
        x <- as.list(friendlyNames)
        names(x) <- categoryN   
    }
    x
}
getInvertedCategoryNames <- function(plural=FALSE) getCategoryNames(plural = plural, invert = TRUE)

# retrieve the group/type assignment for a specific Sample Set as a data.table
# optionally, filter for matching categories
getSampleSetAssignments <- function(id, category1=NULL, category2=NULL, categoryNames=FALSE){
    stepName <- appStepNamesByType$assign
    sampleSet <- app[[stepName]]$outcomes$sampleSets()[[id]]
    assignments <- data.table(sampleSet$assignments)
    if(!is.null(category1)) assignments <- assignments[Category1 == category1]
    if(!is.null(category2)) assignments <- assignments[Category2 == category2]
    if(categoryNames) assignments[, ':='(
        Category1Name = sampleSet$categoryNames[[1]][Category1],
        Category2Name = sampleSet$categoryNames[[2]][Category2]
    )]
    assignments
}

# retrieve the display name of one assigned sample
getAssignedSampleName <- function(sample){ # sample is a one row of the assignments() table we wish to match
    uploadName <- appStepNamesByType$upload
    samples <- app[[uploadName]]$outcomes$samples()
    rows <- which(samples$Project   == sample$Project &
                  samples$Sample_ID == sample$Sample_ID)
    getSampleNames(rows = rows)
}
