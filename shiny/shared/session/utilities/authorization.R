#----------------------------------------------------------------------
# enforce the app and file path authorizations declared in 'config/stage2-app.yml'
# based on:
#   - the server-level flag 'serverEnv$REQUIRES_AUTHENTICATION'
#   - the session-level object 'authenticatedUserData'
#----------------------------------------------------------------------

# determine whether a user is allowed to load a specific app
isAuthorizedApp <- function(appName){
    if(!serverEnv$REQUIRES_AUTHENTICATION) return( TRUE ) # allow all apps if not a public server
    auth <- authenticatedUserData$authorization
    if(is.null(auth) || is.null(auth$apps)) return( FALSE )
    if(length(auth$apps) == 1 && auth$apps == "all") return( TRUE )
    appName %in% auth$apps
}

# get the server file paths authorized to an authenticated user
# all paths must be named and described in config/stage2-apps.yml
getAuthorizedServerPaths <- function(rw = "read"){
    if(!serverEnv$REQUIRES_AUTHENTICATION) return(unlist(serverConfig$paths)) # allow all paths if not a public server
    auth <- authenticatedUserData$authorization
    if(is.null(auth) || is.null(auth$paths) || is.null(auth$paths[[rw]])) return( character() )
    paths <- auth$paths[[rw]]
    if(length(paths) == 1 && paths == "all") paths <- names(serverConfig$paths)
    unlist(serverConfig$paths[paths])
}

# get a user's authorized default root, i.e. volume, for shinyFiles
getAuthorizedRootVolume <- function(type){
    if(serverEnv$REQUIRES_AUTHENTICATION){
        auth <- authenticatedUserData$authorization
        if(is.null(auth) || is.null(auth$paths) || is.null(auth$paths[[type]])) return( NULL )
        root <- auth$paths[[type]]
        paths <- names(serverConfig$paths)
        if(!(root %in% paths)) return( NULL )
        root
    } else {
        root <- if(is.null(type)) NULL else serverConfig[[type]]
        if(is.null(root)) root <- names(serverConfig$paths)[1]
        root
    }
}

# check if an authorization flag is set for a user
getAuthorizationFlag <- function(flag){
    if(!serverEnv$REQUIRES_AUTHENTICATION) return(TRUE)
    auth <- authenticatedUserData$authorization
    if(is.null(auth) || is.null(auth[[flag]]) || !is.logical(auth[[flag]])) return( FALSE )
    auth[[flag]]
}
