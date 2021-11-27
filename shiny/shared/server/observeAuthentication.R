#----------------------------------------------------------------------
# handle login and authentication 
#----------------------------------------------------------------------

# initialize the user on the page
if (file.exists(sessionFile)){
    load(sessionFile, envir = sessionEnv)
    headerStatusData$user <- oauth2UserData$user$email
}

# enable the login buttons on the help page
observeEvent(input$oauth2LoginButton, {
    # if(!is.null(oauth2UserData$user)) return(showUserDialog(
    #     'Already logged in',
    #     "You are already logged in.",
    #     size = "s",
    #     type = 'okOnly'
    # ))
    url <- getOauth2RedirectUrl(sessionKey) 
    runjs(paste0('window.location.replace("', url, '")'));
})
observeEvent(input$keyedLoginButton, {
    reportProgress('input$keyedLoginButton')
    state <- list(accessKey = input$accessKeyEntry)
    stateKey <- getAuthenticationStateKey(sessionKey)
    save(state, file = getAuthenticatedSessionFile('state', stateKey))
    url <- sprintf(
        "location.replace(\"%s\");", 
        paste0(serverEnv$SERVER_URL, '?state=', stateKey, '?accessKey=', TRUE)
    )
    runjs(paste0('window.location.replace("', url, '")'));
})

# show help on the login page about external authorization sources
observeEvent(input$showLoginHelp, {
    show('login-help')
})
