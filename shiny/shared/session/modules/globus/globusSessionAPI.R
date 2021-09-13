
#----------------------------------------------------------------------
# session-level access to Globus resources via that service's API
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# add an authenticated user to the MAGC Portal Users group to enable file transfers
#----------------------------------------------------------------------
addToGlobusUsers <- function(user){
    if(!serverEnv$IS_SERVER || !serverEnv$IS_GLOBUS) return()
    reportProgress(user$sub, 'addToGlobusUsers')
    request <- file.path('groups', globusConfig$usersGroup)     
    body <- list(
        "add" = list(
            list(
                "identity_id" = user$sub
            )
        )
    )      
    postGlobusGroups(request, body) 
}

#----------------------------------------------------------------------
# forward the Globus response to a handler function
# NB: Globus cancel action sends handler and state in query string, submit action in 'action' url
#----------------------------------------------------------------------
handleGlobusHelperResponse__ <- function(type){ # common handler for submit and cancel actions
    reportProgress(type, 'handleGlobusHelperResponse')
    
    # verify request state
    if(is.null(queryString$handler) ||
       is.null(queryString$state)) return()      
    stateMatch <- getGlobusStateKey(sessionKey) == queryString$state
    if(!stateMatch) return() 
    state <- getUserSessionState(sessionKey)
    
    # act based on prior page state
    if(is.null(state$bookmark)){ # Globus was invoked from the launch page
        get(queryString$handler)(queryString, state)
    } else { # Globus was invoked from within an active app, restore that bookmarked state
        file <- tempfile()
        write(state$bookmark, file)
        app <- getTargetAppFromBookmark(state$bookmark)$app
        loadRequest(list(app=app, file=list(name="", path=file, type='bookmark')))
        get(queryString$handler)(queryString, state) # handler must ensure proper wait for bookmark load (e.g. by modal confirmation)
    }
}
handleGlobusHelperResponse <- function(){

    # see if a handler was waiting for a successful login response from Globus Auth helper
    if(!is.null(queryString$login)){ 
        reportProgress('handleGlobusHelperResponse: login')
        state <- getUserSessionState(sessionKey)
        if(!is.null(state$loginHandler)) get(state$loginHandler)(sessionKey, state)
        
    # see if we were waiting for a file action response
    } else if(!is.null(queryString$action)){
        url <- parse_url(queryString$action)        
        queryString$handler <<- url$query$handler
        queryString$state   <<- url$query$state
        handleGlobusHelperResponse__('submit')
        
    # or if user canceled out of a Globus helper
    } else if(!is.null(queryString$handler) &&
              !is.null(queryString$state)){
        handleGlobusHelperResponse__('cancel')
    }
}

#----------------------------------------------------------------------
# helper functions to make requests to a Globus API
#----------------------------------------------------------------------
getGlobusAuth <- function(request, principal = 'user'){
    doGlobusMethod('GET', 'auth', request, principal)
}
getGlobusTransfer <- function(request){
    doGlobusMethod('GET', 'transfer', request, 'user')
}
getGlobusGroups <- function(request){
    doGlobusMethod('GET', 'groups', request, 'client')
}
#----------------------------------------------------------------------
postGlobusAuth <- function(request, body, principal = 'user'){
    doGlobusMethod('POST', 'auth', request, principal, body)
}
postGlobusTransfer <- function(request, body){
    doGlobusMethod('POST', 'transfer', request, 'user', body)
}
postGlobusGroups <- function(request, body){
    doGlobusMethod('POST', 'groups', request, 'client', body)
}
#----------------------------------------------------------------------
putGlobusAuth <- function(request, body, principal = 'user'){
    doGlobusMethod('PUT', 'auth', request, principal, body)
}
putGlobusTransfer <- function(request, body){
    doGlobusMethod('PUT', 'transfer', request, 'user', body)
}
#----------------------------------------------------------------------
deleteGlobusAuth <- function(request, body, principal = 'user'){
    doGlobusMethod('DELETE', 'auth', request, principal, body)
}
deleteGlobusTransfer <- function(request, body){
    doGlobusMethod('DELETE', 'transfer', request, 'user', body)
}

#----------------------------------------------------------------------
# make a request to the Globus API, with error handling
#----------------------------------------------------------------------
doGlobusMethod <- function(method, api, request, principal, body=NULL){
    reportProgress(paste(api, request), method)  
    globusData <- switch(
        principal,
        user   = globusUserData,   # per user session
        client = globusClientData, # per server instance
        NULL
    )
    if(is.null(globusData)) { # developer failsafe
        error <- paste('ERROR: unknown principal:', principal)
        reportProgress(error)
        return(list(success=FALSE, contents=error))
    }  
    startSpinner(session, paste('doGlobusMethod', method, api))
    url <- file.path(globusApis[[api]], request)
    auth <- add_headers(
        Authorization = paste("Bearer", globusData$tokens[[api]]$credentials$access_token)
    )
    response <- switch(method,
        GET = GET(
            url = url, 
            config = auth
        ),
        get(method)( # all other methods have same configuration
            url = url, 
            config = auth,
            body = body,
            encode = "json"
        )                
    )
    stopSpinner(session)    
    return(list(success=!http_error(response), contents=content(response))) 
}

