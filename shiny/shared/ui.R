#----------------------------------------------------------------------
# set up the UI dashboard and launch page (i.e. first file upload)
#----------------------------------------------------------------------

# STYLES AND SCRIPTS, loaded into html <head>
htmlHeadElements <- tags$head(
    tags$link(rel = "icon", type = "image/png", href = "logo/favicon-16x16.png"), # favicon
    tags$link(href = "framework.css", rel = "stylesheet", type = "text/css"), # framework js and css
    tags$script(src = "framework.js", type = "text/javascript", charset = "utf-8"),
    if(serverEnv$IS_DEVELOPER){tagList( # enable developer tools
        tags$script(src = "ace/src-min-noconflict/ace.js", type = "text/javascript", charset = "utf-8"),
        tags$script(src = "summernote-0.8.18-dist/summernote-lite.min.js", type = "text/javascript", charset = "utf-8"),
        tags$link(href = "summernote-0.8.18-dist/summernote-lite.min.css", rel = "stylesheet", type = "text/css")
    )} else ""
)

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

# GLOBUS HELP: the tabset panel with Globus help information
globusHelpPanels <- function(){
    tags$div( # page to help get Globus access to engage that data import method
        id = "globus-setup",
        tabBox(                          
            title = NULL,
            width = 12,                            
            tabPanel(
                title = 'Get a Globus Login',
                value = 'setupGlobusAccount',
                includeMarkdown( file.path('static/setup-globus-account.md') ),
                tags$div(style = "font-size: 1.1em; text-align: center; margin-bottom: 15px;",
                    bsButton('globusLoginButton', 'Log in to Globus', style = "primary") 
                )
            ),
            tabPanel(
                title = 'Receive Data from AGC',
                value = 'setupGlobusTransfers',
                includeMarkdown( file.path('static/receive-agc-data.md') )
            ),
            tabPanel(
                title = 'Run the MDI Locally',
                value = 'runMDILocally',
                includeMarkdown( file.path('static/run-mdi-locally.md') )
            )
        )
    )
}

# LAUNCH PAGE ASSEMBLY: called by ui function (below) as needed
getLaunchPage <- function(cookie, restricted = FALSE){

    # enforce content restrictions on first encounter of a new user while providing Globus help
    if(restricted){
        dataImportMenuItem <- ""
        dataImportTabItem  <- tags$div(class = "tab-pane")
    } else {
        dataImportMenuItem <- menuItem(tags$div('1 - Import Data',  class = "app-step"), 
                                       tabName = "dataImport", selected = !restricted)
        dataImportTabItem  <- dataImportTabItem
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
            sidebarMenu(id = "sidebarMenu",
                menuItem(tags$div(class = "app-step", '0 - Globus Setup'), 
                         tabName = "globusSetup", selected = restricted),               
                dataImportMenuItem         
            ),
            htmlHeadElements, # yes, place the <head> content here (even though it seems odd)
            width = "175px" # must be here, not in CSS
        ),
    
        # body content, i.e. panels associated with each dashboard option
        dashboardBody(
            useShinyjs(), # enable shinyjs
            HTML(paste0("<input type=hidden id='sessionNonce' value='",
                        setSessionKeyNonce(cookie$sessionKey), "' />")),
            tabItems(
                tabItem(tabName = "globusSetup",
                        tags$div(class = "text-block", globusHelpPanels())),                
                dataImportTabItem
            )
        )
    )    
}

# LOADING FLOW CONTROL, i.e. page redirects, associated with Globus API interactions
# this is the function called by Shiny RunApp
ui <- function(request){

    # determine what type of page request this is
    #    public servers demand a valid identity
    #    some local instances might want to use Globus (but never ondemand servers, they use the HPC file system)
    cookie <- parseCookie(request$HTTP_COOKIE) # our function in globusAPI.R
    if(!serverEnv$IS_GLOBUS || serverEnv$IS_ONDEMAND) return( getLaunchPage(cookie) )
    queryString <- parseQueryString(request$QUERY_STRING) # httr function

    # Globus OAuth2 code response, handle and redirect to page with stripped url
    if(!is.null(queryString$code)){
        success <- handleGlobusOAuth2Response(cookie$sessionKey, queryString) # includes state check
        if(!success) return( getLaunchPage(cookie, restricted = TRUE) )
        redirect <- sprintf("location.replace(\"%s\");", paste0(serverEnv$SERVER_URL, '?login=1'))
        tags$script(HTML(redirect)) 
        
    # Globus OAuth2 or other error
    } else if(!is.null(queryString$error)){
        getLaunchPage(cookie, restricted = TRUE)    

    # Globus endpoint helper response
    # local redirect due to inconsistent Globus demands for 127.0.0.1 vs. localhost (have different cookies!) 
    } else if((!is.null(queryString$action) || !is.null(queryString$handler)) &&
               is.null(queryString$endpointRedirect) && 
              serverEnv$IS_LOCAL){
        url <- paste0(serverEnv$SERVER_URL, request$QUERY_STRING, '&endpointRedirect=1')
        redirect <- sprintf("location.replace(\"%s\");", url)
        tags$script(HTML(redirect))
 
    # determine whether we have an active session ...
    } else if(is.null(cookie$sessionKey)) { # session is initializing   
    
        # in server mode, session-initialization service sets the HttpOnly session key (can Shiny do this?)
        if(serverEnv$IS_SERVER){
            url <- paste0(serverEnv$SERVER_URL, 'session')
            redirect <- sprintf("location.replace(\"%s\");", url)
            tags$script(HTML(redirect))
            
        # in local mode, server function sets the session key (this cannot set HttpOnly)    
        } else {
            fluidPage(
                useShinyjs(), # page just needs the js code at this point
                HTML(paste0("<input type=hidden id='sessionNonce' value='' />")), # suppress js error
                tags$head(tags$script(src = "framework.js"))
            )          
        } 

    # ... for a known user
    } else if(serverEnv$IS_SERVER && # new public user, show the help page only
              is.null(cookie$hasLoggedIn) &&
              is.null(queryString$login)){
        getLaunchPage(cookie, restricted = TRUE)  

    # check if we have credentials already, server will know how to handle them
    # TODO: check for freshness? (NB: session-level access tokens have 48 hour lifetime)    
    } else {
        if(file.exists(getGlobusSessionFile('session', cookie$sessionKey))){
            getLaunchPage(cookie)            
        } else {
            redirectToGlobusLogin(cookie$sessionKey)
        }
    }
}
