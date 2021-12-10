#----------------------------------------------------------------------
# set up the UI dashboard and launch page (i.e., the interface for first file upload)
#----------------------------------------------------------------------

# STYLES AND SCRIPTS, loaded into html <head>
htmlHeadElements <- tags$head(
    tags$link(rel = "icon", type = "image/png", href = "logo/favicon-16x16.png"), # favicon
    tags$link(href = "framework.css", rel = "stylesheet", type = "text/css"), # framework js and css
    tags$script(src = "framework.js", type = "text/javascript", charset = "utf-8"),
    if(serverEnv$IS_DEVELOPER){tagList( # enable developer tools
        tags$script(src = "ace/src-min-noconflict/ace.js", type = "text/javascript", charset = "utf-8"),
        tags$script(src = "summernote-0.8.18-dist/summernote-lite.min.js", type = "text/javascript", charset = "utf-8"),
        tags$link(href  = "summernote-0.8.18-dist/summernote-lite.min.css", rel = "stylesheet", type = "text/css")
    )} else ""
)

# LOGIN PAGE CONTENT: prompt for user authentication in server mode
loginControls <- function(buttonId, buttonLabel, helpFile, includeKey = FALSE){
    alignment <- if(includeKey) 'left' else 'center'
    tagList(
        tags$div(
            style = paste0("font-size: 1.1em; text-align: ", alignment, "; margin-bottom: 15px;"),
            if(includeKey) div(
                style = "margin-bottom: 15px; font-size: 0.9em;",
                p('Enter Access Key'),
                passwordInput('accessKeyEntry', NULL)
            ) else "",
            bsButton(buttonId, paste("Log in using", buttonLabel), style = "primary"),
            actionLink('showLoginHelp', 'Help', style = "margin-left: 1em; font-size: 0.8em;")
        ),
        tags$div(
            id = "login-help",
            style = "display: none;",
            hr(),
            includeMarkdown( file.path('static', helpFile) )
        )
    )
}
userLoginTabItem <- tabItem(tabName = "loginTab", tags$div(class = "text-block",
    tags$div(
        id = CONSTANTS$apps$loginPage,
        includeMarkdown( file.path('static/mdi-intro.md') ),
        if(serverEnv$IS_GLOBUS) 
            loginControls('oauth2LoginButton', 'Globus', 'globus-help.md')
        else if(serverEnv$IS_GOOGLE)
            loginControls('oauth2LoginButton', 'Google', 'google-help.md')
        else if(serverEnv$IS_KEYED) 
            loginControls('keyedLoginButton',  'Key',    'access-key-help.md', includeKey = TRUE)
        else p("CONFIGURATION ERROR")
    )
))

# LAUNCH PAGE CONTENT: the first items typically seen that support data import
dataImportTabItem <- tabItem(tabName = "dataImport", tags$div(class = "text-block",
    tags$div( # main launch page and file upload
        id = CONSTANTS$apps$launchPage,
        style = "display: none;", # server.R selects the proper content to show
        includeMarkdown( file.path('static/mdi-intro.md') ),
        uiOutput('mainFileInputUI'),
        if(serverEnv$IS_WINDOWS) ""  
        else includeMarkdown( file.path('static/launch-page-stage1.md') ), # OS dependent
             includeMarkdown( file.path('static/launch-page-stage2.md') ), # always present
        uiOutput('bookmarkHistoryList')
    ),
    tags$div( # server busy page
        id = "server-busy",
        style = "display: none;",
        includeMarkdown( file.path('static/server-busy.md') )
    ),
    plotlyOutput('nullPlotly', height = "0px", width = "0px") # must do now so plotly.js etc. are loaded  
))

# LAUNCH PAGE ASSEMBLY: called by ui function (below) as needed
getLaunchPage <- function(cookie, restricted = FALSE){

    # enforce content restrictions on first encounter of a new user while providing login help
    if(restricted){
        firstMenuItem <- menuItem(tags$div('1 - Login',  class = "app-step"), tabName = "loginTab")
        firsttTabItem <- userLoginTabItem
    } else {
        firstMenuItem <- menuItem(tags$div('1 - Import Data',  class = "app-step"), tabName = "dataImport")
        firsttTabItem <- dataImportTabItem
    }
    
    # assemble the dashboard page style
    dashboardPage(
        dashboardHeader(
            title = "MDI",
            titleWidth = "175px"
            #dropdownMenu(type = "messages", badgeStatus = "success",
            #    messageItem("Support Team", "This is the content of a message.", time = "5 mins")
            #),
            #dropdownMenu(type = "notifications", badgeStatus = "warning",
            #    notificationItem(icon = icon("users"), status = "info", "5 new members joined today")
            #),
            #dropdownMenu(type = "tasks", badgeStatus = "danger",
            #    taskItem(value = 20, color = "aqua", "Refactor code")
            #)
        ),
        dashboardSidebar(
            # the dashboard option selectors, filled dynamically per app after first data load
            sidebarMenu(id = "sidebarMenu", firstMenuItem),
            htmlHeadElements, # yes, place the <head> content here (even though it seems odd)
            width = "175px" # must be here, not in CSS
        ),
    
        # body content, i.e., panels associated with each dashboard option
        dashboardBody(
            useShinyjs(), # enable shinyjs
            HTML(paste0("<input type=hidden id='sessionNonce' value='",
                        setSessionKeyNonce(cookie$sessionKey), "' />")),
            tabItems(firsttTabItem)
        )
    )    
}

# AUTHENTICATION FLOW CONTROL, i.e., page redirects, associated with authentication interactions
handleLoginResponse <- function(cookie, queryString, handler){
    success <- handler(cookie$sessionKey, queryString)
    getLaunchPage(cookie, restricted = !success)
}
parseAuthenticationRequest <- function(request, cookie){
    queryString <- parseQueryString(request$QUERY_STRING) # parseQueryString is an httr function

    # handle accessKey submission
    if(!is.null(queryString$accessKey)){
        handleLoginResponse(cookie, queryString, handleAccessKeyResponse)

    # handle OAuth2 code response
    } else if(!is.null(queryString$code)){
        handleLoginResponse(cookie, queryString, handleOauth2Response)
        
    # OAuth2 or other error
    } else if(!is.null(queryString$error)){
        getLaunchPage(cookie, restricted = TRUE)
 
    # determine whether we have an active session ...
    } else if(is.null(cookie$sessionKey)) { # session-initialization service sets the HttpOnly session key
        url <- paste0(serverEnv$SERVER_URL, 'session')
        redirect <- sprintf("location.replace(\"%s\");", url)
        tags$script(HTML(redirect))

    # ... for a known user
    } else if(serverEnv$REQUIRES_AUTHENTICATION && # new session for a public user, show the login page only
              is.null(cookie$hasLoggedIn)){ # false if reloading a current valid session
        getLaunchPage(cookie, restricted = TRUE)

    # check if we have credentials already, server will know how to handle them  
    } else {
        if(file.exists(getAuthenticatedSessionFile('session', cookie$sessionKey))){ # non-cookie check for an authenticated session # nolint
            getLaunchPage(cookie)
        } else if(serverEnv$IS_KEYED){
            getLaunchPage(cookie, restricted = TRUE)
        } else {
            redirectToOauth2Login(cookie$sessionKey) # allow oauth2 users to quickly pass authentication via prior SSO
        }
    }
}

# MAIN UI FUNCTION: determine what type of page request this is and act accordingly
# this is the function called by Shiny RunApp
ui <- function(request){
    cookie <- parseCookie(request$HTTP_COOKIE) # parseCookie is an MDI-encoded helper function
    if(serverEnv$REQUIRES_AUTHENTICATION){ # public servers demand a valid identity
        parseAuthenticationRequest(request, cookie)
    } else {
        getLaunchPage(cookie)
    }
}
