
#----------------------------------------------------------------------
# retrieve analysis schema names, unique identifiers, statuses
#----------------------------------------------------------------------
# depends on runAnalyses or comparable replacement module
# which must set app[[stepName]]$outcomes$analyses and $analysisNames reactives
#----------------------------------------------------------------------
# names can be overriden by user edits and are stored in keyed list
#----------------------------------------------------------------------

# get the full definition of a specific analysisType
getAnalysisType <- function(typeName){
    stepName <- appStepNamesByType$analyze
    app[[stepName]]$analysisTypes[[typeName]]
}

# get the list of analysis types
getAnalysisTypeNames <- function(){
    stepName <- appStepNamesByType$analyze
    app[[stepName]]$analysisTypeNames
}

# get a single analysis name, with overrides
getSchemaName <- function(id){
    stepName <- appStepNamesByType$analyze
    outcomes <- app[[stepName]]$outcomes
    name <- outcomes$schemaNames()[[id]]
    if(is.null(name)) outcomes$schema()[[id]]$name # systematic
    else name # as assigned by user
}

# get the list of analysis names with user overrides
getSchemaNames <- function(rows=TRUE){
    stepName <- appStepNamesByType$analyze
    sapply(names(app[[stepName]]$outcomes$schema())[rows], getSchemaName)
}

# get the category axes we might group by for a comparison analysis
getAnalyzeByNames <- function(invert=FALSE){
    getCategoryNames(plural=FALSE, invert=invert)
}
getInvertedAnalyzeByNames <- function(){
    getCategoryNames(plural=FALSE, invert=TRUE)
}

# get a single analysis status
getAnalysisStatus <- function(id){
    stepName <- appStepNamesByType$analyze
    app[[stepName]]$outcomes$schema()[[id]]$status
}

# get the list of analysis statuses
getAnalysisStatuses <- function(){
    stepName <- appStepNamesByType$analyze
    if(is.null(app[[stepName]])) return(c())
    schema <- app[[stepName]]$outcomes$schema()
    if(is.null(schema)) return(c())
    ids <- names(schema)
    if(length(ids) == 0) c() else sapply(ids, getAnalysisStatus)
}

# return analyses that have completed successfully
getFilteredAnalyses <- function(status=NULL){ # status is a NULL or string 'success',etc.
    stepName <- appStepNamesByType$analyze
    req(app[[stepName]])
    schema <- app[[stepName]]$outcomes$schema()    
    req(schema)
    if(length(schema) == 0) return(NULL)
    if(is.null(status)) return(schema)
    status <- CONSTANTS$jobStatuses[[status]]$value
    matches <- as.logical(sapply(schema, function(x) x$status == status))
    schema[matches]
}
getSuccessfulAnalyses <- function(){
    getFilteredAnalyses(status = 'success')
}
areSuccessfulAnalyses <- function(){
    x <- getSuccessfulAnalyses()
    !is.null(x) && length(x) > 0
}

