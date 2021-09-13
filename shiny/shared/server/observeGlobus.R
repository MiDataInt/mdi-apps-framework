#----------------------------------------------------------------------
# handle Globus login and authentication 
#----------------------------------------------------------------------

# initialize the user on the page
if (file.exists(sessionFile)){
    # TODO: need to check keys for freshness?
    # NB: not using refresh tokens (but see globusClient.R), access tokens have 48 hour lifetime
    load(sessionFile, envir = sessionEnv)
    headerStatusData$user <- globusUserData$user$email
    addToGlobusUsers(globusUserData$user)
    handleGlobusHelperResponse()
}
    
# enable the Globus login button on the help page
observeEvent(input$globusLoginButton, {
    if(!is.null(globusUserData$user)) return(showUserDialog(
        'Already logged in',
        "You are already logged in to Globus and the MAGC Portal",
        size="s",
        type='okOnly'
    ))
    url <- getGlobusRedirectUrl(sessionKey) 
    runjs(paste0('window.location.replace("', url, '")'));
})

## from str(globusUserData)
# $ tokens       :List of 2
#  ..$ auth    :Classes 'Token2.0', 'Token', 'R6' <Token2.0>
#  Inherits from: <Token>
#  Public:
#    app: oauth_app
#    cache: function (path = self$cache_path) 
#    cache_path: NULL
#    can_refresh: function () 
#    clone: function (deep = FALSE) 
#    credentials: list
#    endpoint: oauth_endpoint
#    hash: function () 
#    init_credentials: function () 
#    initialize: function (app, endpoint, params = list(), credentials = NULL, 
#    load_from_cache: function () 
#    params: list
#    print: function (...) 
#    private_key: NULL
#    refresh: function () 
#    revoke: function () 
#    sign: function (method, url) 
#    validate: function ()

## first addition of user to group
#$success
#[1] TRUE
#$contents
#$contents$add
#$contents$add[[1]]
#$contents$add[[1]]$group_id
#[1] "36906322-801b-11eb-bfb2-0abec420f939"
#$contents$add[[1]]$identity_id
#[1] "c43a05e1-xxxx"
#$contents$add[[1]]$username
#[1] "wilsonte@umich.edu"
#$contents$add[[1]]$role
#[1] "member"
#$contents$add[[1]]$status
#[1] "active"
#
## no error if we try again (and would have to poll for membership anyway)
#$success
#[1] TRUE
#$contents
#$contents$add
#list()
#$contents$errors
#$contents$errors$add
#$contents$errors$add[[1]]
#$contents$errors$add[[1]]$identity_id
#[1] "c43a05e1-xxxx"
#$contents$errors$add[[1]]$code
#[1] "ALREADY_ACTIVE"
#$contents$errors$add[[1]]$detail
#[1] "The identity is already an active member of the group."

