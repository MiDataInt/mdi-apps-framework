#----------------------------------------------------------------------
# execute OAuth2 authorization code grant via httr
# applies to user authorization of web app for login
#----------------------------------------------------------------------

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

# initialize OAuth2 by redirecting user to auth and login
redirectToOauth2Login <- function(sessionKey, state = list()){
    redirect <- sprintf("location.replace(\"%s\");", getOauth2RedirectUrl(sessionKey, state))
    tags$script(HTML(redirect))
}
# called by server.R, only has hashed sessionKey, i.e., stateKey
getOauth2RedirectUrl <- function(sessionKey, state = list()){
    stateKey <- getAuthenticationStateKey(sessionKey)
    save(state, file = getAuthenticatedSessionFile('state', stateKey))
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
    stateMatch <- getAuthenticationStateKey(sessionKey) == queryString$state
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
        save(oauth2UserData, file = getAuthenticatedSessionFile('session', sessionKey)) # cache user session by sessionKey # nolint
    } else {
        message('!! OAuth2 state check failed !!')   
    }
    stateMatch
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
