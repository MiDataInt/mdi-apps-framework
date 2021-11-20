#----------------------------------------------------------------------
# handle login and authentication 
#----------------------------------------------------------------------

# initialize the user on the page
if (file.exists(sessionFile)){
    load(sessionFile, envir = sessionEnv)
    headerStatusData$user <- oauth2UserData$user$email
}
    
# enable the login button on the help page
observeEvent(input$oauth2LoginButton, {
    if(!is.null(oauth2UserData$user)) return(showUserDialog(
        'Already logged in',
        "You are already logged in.",
        size = "s",
        type = 'okOnly'
    ))
    url <- getOauth2RedirectUrl(sessionKey) 
    runjs(paste0('window.location.replace("', url, '")'));
})
