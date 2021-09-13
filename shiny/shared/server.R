#----------------------------------------------------------------------
# server function is called once per user session by run_framework.R > Shiny::runApp
# nearly all other scripts are sourced by the serverFn function
#----------------------------------------------------------------------

# called by server function (below) based on priorCookie status
serverFn <- function(input, output, session,
                     sessionKey, sessionFile,
                     cookie, restricted=FALSE){
    queryString <- parseQueryString(isolate(session$clientData$url_search))
    if(length(queryString) > 0) updateQueryString("?", mode="push") # clear the url

    # public servers demand user authentication; ui is redirecting
    if(serverEnv$IS_SERVER &&  # allow all local page loads
       is.null(queryString$code) && # user has successfully logged in with Globus, this is an auth response
       !file.exists(sessionFile) && # this is a session with prior auth
       !restricted # this is a user's first encounter, show login help
    ) return()
    
    # source the code that defines a session
    source("server/initializeSession.R", local=TRUE)
    show(if(MbRAM_beforeStart > serverEnv$MAX_MB_RAM_BEFORE_START)
         CONSTANTS$apps$serverBusy else CONSTANTS$apps$launchPage)        
    createSpinner() # create the loading spinner
    source("server/initializeLaunchPage.R", local=TRUE)
    source("server/observeLoadRequest.R", local=TRUE)
    source("server/onSessionEnded.R", local=TRUE)
    source("server/observeGlobus.R", local=TRUE) # last code acts on login
}

#----------------------------------------------------------------------
# set/get session cookie and act on its values
# this server function must come last in server.R
#----------------------------------------------------------------------
server <- function(input, output, session){

    # send message to javascript to set the session key (won't override an existing session)
    session$sendCustomMessage('initializeSession', list(
        value = nonce(),
        isServerMode = serverEnv$IS_SERVER
    ))
    
    # listen for the response and act accordingly
    initializationObserver <- observeEvent(input$initializeSession, {
        initializationObserver$destroy()
        
        # parse information from js
        cookie <- parseCookie(input$initializeSession$cookie)
        priorCookie <- parseCookie(input$initializeSession$priorCookie)
        sessionKey <- getSessionKeyFromNonce(input$initializeSession$sessionNonce) # cannot rely on having sessionKey if HttpOnly
        sessionFile <- getGlobusSessionFile('session', sessionKey)
        isLoggedIn <- file.exists(sessionFile)        
        
        # new public user, show the help page only
        if(serverEnv$IS_SERVER && 
           is.null(cookie$hasLoggedIn) &&
           !isLoggedIn) { 
            serverFn(input, output, session,
                     sessionKey, sessionFile,
                     cookie, restricted=TRUE)
            
        # redirect after setting session key in local mode
        } else if(is.null(priorCookie$isSession)){ 
            runjs(paste0('window.location.replace("', serverEnv$SERVER_URL, '")'));
            
        # definitive page load
        } else {
            if(isLoggedIn && is.null(cookie$hasLoggedIn)) session$sendCustomMessage(
                'setDocumentCookie',
                list(
                    name  = 'hasLoggedIn',
                    data  = list(value = 1, isServerMode = serverEnv$IS_SERVER),
                    nDays = 10 * 365
                )
            )
            serverFn(input, output, session,
                     sessionKey, sessionFile,
                     cookie, restricted=FALSE) 
        }
    })
}

