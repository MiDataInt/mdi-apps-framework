
#----------------------------------------------------------------
# structure of the Portal framework, i.e encompassing all apps
#----------------------------------------------------------------
# apps reside according to structure .../shiny/apps/<familyName>/<appName>
#----------------------------------------------------------------

#getFrameworkApps <- function(){
#    knownApps <- list() # name = appName, value = familyName
#    appFamilies <- list.dirs(path=paste('..', 'apps', sep="/"), full.names=FALSE, recursive=FALSE)
#    for(familyName in appFamilies){ 
#        appNames <- list.dirs(path=paste('..', 'apps', familyName, sep="/"), full.names=FALSE, recursive=FALSE)
#        currentAppNames <- names(knownApps)
#        knownApps <- c(knownApps, rep(familyName, length(appNames)))
#        names(knownApps) <- c(currentAppNames, appNames)
#    }
#    knownApps
#}

