
# add an app step in local mode to facilitate code exploration
addDeveloperMenuItem <- function(){
    if( serverEnv$IS_SERVER || # !! never show developer tools in server mode !!
       !serverEnv$IS_DEVELOPER) return(NULL)
    app$config$appSteps <<- append(app$config$appSteps, list(
        develop = list(
            shortLabel = "Develop", 
            shortDescription = "Tools to allow app developers to write, debug and manage code",
            module = "developerTools",
            options = list(
                #source = 'analyze',
                alwaysVisible = TRUE
            )
        )                                   
    ))
}

