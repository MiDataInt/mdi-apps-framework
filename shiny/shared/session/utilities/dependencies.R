#----------------------------------------------------------------------
# enforce data integrity as parent-child relationships
#----------------------------------------------------------------------
#  this only applies to UI state, not a deeper underlying database or other data structure
#      e.g., removing a manifest just removes it from the page state
#      but does NOT delete it permanently from the disk, etc.
#----------------------------------------------------------------------

# at session start, create reactiveValues for each app step to record its data locks
intializeStepLocks <- function(){
    locks <- list()
    for(stepName in names(app$config$appSteps)) locks[[stepName]] <- reactiveValues()
    locks
}

# use stepModuleInfo sourceTypes to tag parents with the children that depend on them
#   only necessary to tag descendants one level-deep (i.e. children) since locks create a chain
initializeDescendants <- function(){
    steps <- app$config$appSteps
    nSteps <- length(steps)
    if(nSteps <= 1) return(NULL)
    stepIs <- 1:nSteps
    for(i in stepIs){
        parentName <- names(steps)[i]
        app$config$appSteps[[i]]$descendants <- c()
        for(j in stepIs){
            childName <- names(steps)[j]
            childSourceTypes <- stepModuleInfo[[steps[[j]]$module]]$sourceTypes
            if(is.null(childSourceTypes)) childSourceTypes <- c()
            for(childSourceType in childSourceTypes){
                childSource <- getAppStepNameByType(childSourceType)
                if(childSource == parentName){
                    app$config$appSteps[[i]]$descendants <- append(
                        app$config$appSteps[[i]]$descendants, childName
                    ) 
                }
            }
        } 
    }
}

# help a child place locks on its parent on child record create
placeRecordLocks <- function(stepLocks, module, childId, parentIds){
    reportProgress('placeRecordLocks', module)
    for(parentId in unique(parentIds)){
        if(is.null(stepLocks[[parentId]])) stepLocks[[parentId]] <- character()
        stepLocks[[parentId]] <- unique(c(stepLocks[[parentId]], childId))
    }
}

# help a child clear locks on its parent on child record delete
clearRecordLocks <- function(stepLocks, module, childId, parentIds){
    reportProgress('clearRecordLocks', module)
    for(parentId in unique(parentIds)){
        keep <- stepLocks[[parentId]] != childId
        stepLocks[[parentId]] <- stepLocks[[parentId]][keep]
    }
}
