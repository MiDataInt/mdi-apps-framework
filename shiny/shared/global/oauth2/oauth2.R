#----------------------------------------------------------------------
# oauth2/httr helper functions
#----------------------------------------------------------------------

# make a ~unique key (could also use UUID package)
nonce <- function(){ 
    digest(paste(
        Sys.time(),
        paste0(sample(c(letters, LETTERS, 0:9), 50), collapse = "")
    ))
}

# get 'code', 'state' and other flow control parameters from query string
parseCookie <- function(cookie){ 
    nullCookie <- list()
    if(is.null(cookie) || cookie == "") return( nullCookie )
    keyValuePairs <- strsplit(cookie, '; ')[[1]]
    if(length(keyValuePairs) == 0) return( nullCookie )
    cookie <- list()
    for(kvp in keyValuePairs){
        kvp <- strsplit(kvp, '=')[[1]]
        cookie[[kvp[1]]] <- kvp[[2]]
    }
    cookie # a named list, similar to parseQueryString() 
}

# standardized session file paths
getOauth2SessionFile <- function(type, key){ 
    if(is.null(key)) key <- "XXXXXXXX"
    file.path(serverEnv$SESSIONS_DIR, paste(type, key, sep = "-"))
}

# return the proper client and API data based on server config
getOauth2Config <- function(){
    if(serverEnv$IS_GLOBUS){ list(
        endpoint  = globusAuthEndpoints,
        app       = globusClient,
        scope     = globusUserScopes,
        publicKey = globusPublicKey,
        urls      = globusHelperPages
    )} else if(serverEnv$IS_GOOGLE) { list(

    )}
}

# convert tokens
convertOauth2Tokens <- function(tokens){
    config <- getOauth2Config()
    oauth2.0_token(
        endpoint    = config$endpoint,
        app         = config$app,
        credentials = tokens,
        cache = FALSE # handled elsewhere
    )
}

#----------------------------------------------------------------------
# execute OAuth2 authorization code grant via httr
# applies to user authorization of web app for login
#----------------------------------------------------------------------

# derive a unique and opaque one-way hash of a signed sessionKey for use as OAuth2 state parameter
getOauth2StateKey <- function(sessionKey){
    digest(paste(sessionKey, serverId))
}
getUserSessionState <- function(sessionKey){
    stateKey <- getOauth2StateKey(sessionKey)
    stateFile <- getOauth2SessionFile('state', stateKey)
    if(!file.exists(stateFile)) return(list())
    load(stateFile)
    state
}

# initialize OAuth2 by redirecting user to auth and login
redirectToOauth2Login <- function(sessionKey, state = list()){
    redirect <- sprintf("location.replace(\"%s\");", getOauth2RedirectUrl(sessionKey, state))
    tags$script(HTML(redirect))
}
# called by server.R, only has hashed sessionKey, i.e., stateKey
getOauth2RedirectUrl <- function(sessionKey, state = list()){
    stateKey <- getOauth2StateKey(sessionKey)
    save(state, file = getOauth2SessionFile('state', stateKey))
    config <- getOauth2Config()
    oauth2.0_authorize_url(
        endpoint = config$endpoint,
        app      = config$app, 
        scope    = config$scope,
        state    = stateKey
    )
}

# process the OAuth2 code response by turning it into tokens
handleOauth2Response <- function(sessionKey, queryString){
    stateMatch <- getOauth2StateKey(sessionKey) == queryString$state
    if(stateMatch){ # validate state to prevent cross site forgery
        config <- getOauth2Config()
        tokens <- oauth2.0_access_token(    # completes the OAuth2 authorization sequence
            endpoint = config$endpoint, # returns different tokens on each call
            app      = config$app,        # access tokens have expires_in = 172800 seconds = 48 hours
            code     = queryString$code
        )
        oauth2UserData <- list(tokens = list( # sets into variable declared is server function, i.e., session scoped
            auth     = convertOauth2Tokens(tokens)
        ))
        oauth2UserData$user <- jwt_decode_sig(tokens$id_token, config$publicKey) # using the id_token
        save(oauth2UserData, file = getOauth2SessionFile('session', sessionKey)) # cache user session by sessionKey
    } else {
        message('!! OAuth2 state check failed !!')   
    }
    stateMatch
}
