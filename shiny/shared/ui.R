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

# LOGIN PAGE CONTENT: prompt user authentication in server mode
userLoginTabItem <- tabItem(tabName = "loginTab", tags$div(class = "text-block",
    tags$div(
        id = CONSTANTS$apps$loginPage,
        includeMarkdown( file.path('static/mdi-intro.md') ),
        if(serverEnv$IS_GLOBUS || TRUE) tagList(
            tags$div(style = "font-size: 1.1em; text-align: center; margin-bottom: 15px;",
                bsButton('oauth2LoginButton', 'Log in using Globus', style = "primary"),
                actionLink('showOAuth2Help', 'Help', style = "margin-left: 1em; font-size: 0.8em;")
            ),
            tags$div( # server busy page
                id = "oauth2-help",
                style = "display: none;",
                includeMarkdown( file.path('static/globus-help.md') )
            )
        ) else if(serverEnv$IS_GOOGLE) "PENDING"
        else "PENDING"
    )
))

# LAUNCH PAGE CONTENT: the first items typically seen that support data import
dataImportTabItem <- tabItem(tabName = "dataImport", tags$div(class = "text-block",
    tags$div( # main launch page and file upload
        id = CONSTANTS$apps$launchPage,
        style = "display: none;", # server.R selects the proper content to show
        includeMarkdown( file.path('static/mdi-intro.md') ),
        uiOutput('mainFileInputUI'),
        includeMarkdown( file.path('static/launch-page.md') ),
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

# LOADING FLOW CONTROL, i.e., page redirects, associated with Oauth2 API interactions
parseAuthenticationRequest <- function(request, cookie){
    queryString <- parseQueryString(request$QUERY_STRING) # httr function

    # user was redirected back after logging out; reset to login page
    if(!is.null(queryString$logout)){
        getLaunchPage(cookie, restricted = TRUE)   

    # OAuth2 code response, handle and redirect to page with stripped url
    } else if(!is.null(queryString$code)){
        success <- handleOauth2Response(cookie$sessionKey, queryString) # includes state check
        if(!success) return( getLaunchPage(cookie, restricted = TRUE) )
        redirect <- sprintf("location.replace(\"%s\");", paste0(serverEnv$SERVER_URL, '?login=1'))
        tags$script(HTML(redirect)) 
        
    # OAuth2 or other error
    } else if(!is.null(queryString$error)){
        getLaunchPage(cookie, restricted = TRUE)    
 
    # determine whether we have an active session ...
    } else if(is.null(cookie$sessionKey)) { # session is initializing   
    
        # session-initialization service sets the HttpOnly session key (can Traefik or Shiny do this?)
        url <- paste0(serverEnv$SERVER_URL, 'session')
        redirect <- sprintf("location.replace(\"%s\");", url)
        tags$script(HTML(redirect))

    # ... for a known user
    } else if(serverEnv$IS_SERVER && # new public user, show the help page only
              is.null(cookie$hasLoggedIn) &&
              is.null(queryString$login)){
        getLaunchPage(cookie, restricted = TRUE)  

    # check if we have credentials already, server will know how to handle them  
    } else {
        if(file.exists(getOauth2SessionFile('session', cookie$sessionKey))){
            getLaunchPage(cookie)            
        } else {
            redirectToOauth2Login(cookie$sessionKey)
        }
    }
}

# determine what type of page request this is and act accordingly
# this is the function called by Shiny RunApp
ui <- function(request){
    cookie <- parseCookie(request$HTTP_COOKIE) # our helper function in oauth2.R
    if(serverEnv$REQUIRES_AUTHENTICATION){
        parseAuthenticationRequest(request, cookie) # public servers demand a valid identity
    } else {
        return( getLaunchPage(cookie) )
    }
}
