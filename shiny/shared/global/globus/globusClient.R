#----------------------------------------------------------------------
# define the web application client acting on behalf of a user
#----------------------------------------------------------------------
globusClient <- oauth_app(
    appname = "globus",
    key     = globusConfig$client$key,
    secret  = globusConfig$client$secret,
    redirect_uri = serverEnv$SERVER_URL
)
globusUserScopes <- paste( # the user grants MAGC Portal permissions to ...
    "openid", # ... read their identifying information
    "email",      # includes $sub and $email for identity and linked identities in $identity_set
    "profile",    # profile adds $name (e.g. John Doe) and $identity_provider[_display_name]
    "urn:globus:auth:scope:transfer.api.globus.org:all" # ... make transfer request that they configure
)
globusClientScopes <- paste( # a public server grants its client application permissions to ...
    "urn:globus:auth:scope:groups.api.globus.org:all", # ... assign authenticated users to MAGC Portal Users group
    sep="+"
)

#----------------------------------------------------------------------
# execute Globus OAuth2 client credentials grant to enable the client to add users to MAGC Portal Users group
# NB: refresh tokens do not apply to client credential grants; simply recall as needed
#----------------------------------------------------------------------
getGlobusClientCredentials <- function(){
    if(is.null(globusConfig$client$key) ||
       is.null(globusConfig$client$secret)){
        print("!! ERROR: expected client application key and secret in server mode !!")
        return(NULL)
    }
    body <- paste( # a URL-encoded web form
        paste('scope', globusClientScopes, sep='='),
        paste('grant_type', 'client_credentials', sep='='),
        sep = "&"
    )
    response <- POST(
        url = file.path(globusApis$auth, 'v2/oauth2/token'),    
        authenticate(globusClient$key, globusClient$secret, type = "basic"),
        content_type('application/x-www-form-urlencoded'),
        body = body
    )
    if(http_error(response)){
        print(http_status(response))
        NULL
    } else { # function called by run_framework.R, so client credentials are server-scoped
        tokens <- content(response)
        list( groups = convertGlobusTokens(tokens) )
    }    
}

#----------------------------------------------------------------------
# execute Globus OAuth2 authorization code grant via httr
# applies to user authorization of web app for login and transfer (client uses its own credentials above)
#----------------------------------------------------------------------

# derive a unique and opaque one-way hash of a signed sessionKey for use as OAuth2 state parameter
getGlobusStateKey <- function(sessionKey){
    digest(paste(sessionKey, globusConfig$signature))
}
getUserSessionState <- function(sessionKey){
    stateKey <- getGlobusStateKey(sessionKey)
    stateFile <- getGlobusSessionFile('state', stateKey)
    if(!file.exists(stateFile)) return(list())
    load(stateFile)
    state
}

# initialize OAuth2 by redirecting user to Globus auth and login
redirectToGlobusLogin <- function(sessionKey, state = list()){
    redirect <- sprintf("location.replace(\"%s\");", getGlobusRedirectUrl(sessionKey, state))
    tags$script(HTML(redirect))
}
# called by server.R, only has hashed sessionKey, i.e. stateKey
getGlobusRedirectUrl <- function(sessionKey, state = list()){
    stateKey <- getGlobusStateKey(sessionKey)
    save(state, file = getGlobusSessionFile('state', stateKey))
    oauth2.0_authorize_url(
        endpoint = globusAuthEndpoints,
        app      = globusClient, 
        scope    = globusUserScopes,
        state    = stateKey
        #, query_extra = list(access_type = "offline") # to get refresh token too; access tokens last 48 hours
    )
}

# process the OAuth2 code response by turning it into tokens
handleGlobusOAuth2Response <- function(sessionKey, queryString){
    stateMatch <- getGlobusStateKey(sessionKey) == queryString$state
    if(stateMatch){ # validate state to prevent cross site forgery
        tokens <- oauth2.0_access_token(    # completes the OAuth2 authorization sequence
            endpoint = globusAuthEndpoints, # returns different tokens on each call
            app      = globusClient,        # access tokens have expires_in = 172800 seconds = 48 hours
            code     = queryString$code
        )
        globusUserData <- list(tokens = list( # sets into variable declared is server function, i.e. session scoped
            auth     = convertGlobusTokens(tokens),
            transfer = convertGlobusTokens(tokens$other_tokens[[1]]) # transfer token is distinct, in 'other_tokens'
        ))
        #user <- GET( # using the API
        #    url = file.path(globusApis$auth, '/v2/oauth2/userinfo'), 
        #    config = add_headers(
        #        Authorization = paste("Bearer", globusUserData$tokens$auth$credentials$access_token)
        #    )
        #)        
        #globusUserData$user <- if(http_error(user)) NULL else content(user)
        globusUserData$user <- jwt_decode_sig(tokens$id_token, globusPublicKey) # using the id_token (better)
        save(globusUserData, file = getGlobusSessionFile('session', sessionKey)) # cache user session by sessionKey
    } else {
        message('!! Globus state check failed !!')   
    }
    stateMatch
}

#----------------------------------------------------------------------
# execute Globus file transfer via httr
#----------------------------------------------------------------------

# redirect user to Globus file manager with Submit button
# state$handler is called when the result returns to MAGC Portal
redirectToGlobusFileManager <- function(sessionKey, state){ # state = list with at least handler, folderlimit, filelimit
    stateKey <- getGlobusStateKey(sessionKey)
    save(state, file = getGlobusSessionFile('state', stateKey))
    callbackUrl <- parse_url(serverEnv$CALLBACK_URL)
    callbackUrl$query <- list(handler = state$handler, state=stateKey)
    callbackUrl <- build_url(callbackUrl)
    redirectUrl <- parse_url(globusHelperPages$browseEndpoint)     
    redirectUrl$query <- list(
        method = 'GET',      
        action = callbackUrl,
        cancelurl = callbackUrl,
        folderlimit = state$folderlimit, # NB: code allows 1 file or 1 folder only
        filelimit = state$filelimit      # actually, limits don't seem to work?
    )
    runjs(paste0("window.location.href = '", build_url(redirectUrl),"'"))
}

