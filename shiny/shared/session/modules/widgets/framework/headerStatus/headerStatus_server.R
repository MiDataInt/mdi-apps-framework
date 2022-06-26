#----------------------------------------------------------------------
# reactive components for feedback on current user and dataDir
# also OAuth2 logout when relevant
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
headerStatusServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        
# output text
output$userDisplayName <- renderText({ 
    name <- headerStatusData$userDisplayName
    domain <- serverEnv$MDI_REMOTE_DOMAIN
    if(is.null(domain)) name else paste(name, domain, sep="@")
})
output$dataDir <- renderText({ 
    req(headerStatusData$userDisplayName)
    headerStatusData$dataDir 
})    

# allow remote user to unlock the MDI installation, i.e., all frameworks and suites
observeEvent(input$unlockAllRepos, {
    req(headerStatusData$userDisplayName)
    showUserDialog(
        "Unlock MDI Installation", 
        tags$p(paste("Please click OK to confirm that you wish to remove all framework and suite lock files from your local or remote MDI installation.")),
        tags$p(serverEnv$MDI_DIR, style = "margin-left: 2em;"),
        tags$p("This action is usually only required if you experienced a fatal error during execution of an MDI command, e.g., in Pipeline Runner."),
        callback = function(...) {
            sapply(c(
                file.path(serverEnv$MDI_DIR, "suites", "*.lock"),
                file.path(serverEnv$MDI_DIR, "frameworks", "*.lock")
            ), unlink, force = TRUE)
        },
        size = "m", 
        type = 'okCancel'
    )   
})

# allow user to log out
observeEvent(input$logout, {
    req(headerStatusData$userDisplayName)
    config <- getOauth2Config()
    url <- parse_url(config$urls$logout)
    file.remove(sessionFile) # make sure we forget them too
    url$query <- list(
        client_id = serverConfig$oauth2$client$key, # don't redirect back to app, since it requires logging in again!
        redirect_uri = serverEnv$SERVER_URL,
        redirect_name = paste("Michigan Data Interface at", serverEnv$SERVER_URL)
    )
    runjs(paste0("window.location.href = '", build_url(url), "'"))
})

# allow user to change the dataDir; necessarily restarts the server
serverChooseDirIconServer(
    'changeDataDir', 
    input, 
    session,
    chooseFn = function(dir){
        req(dir)
        dir <- dir$dir
        req(dir)
        if(dir == serverEnv$DATA_DIR) return(NULL)
        if(endsWith(dir, 'mdi/data')){
            showUserDialog(
                "Server Restart Required", 
                tags$p(paste("The server must restart to change data directories.")),
                tags$p("Please reload a fresh web page to start a new session once the server restarts."),
                callback = function(...) {
                    serverEnv$DATA_DIR <<- dir
                    Sys.setenv(MDI_FORCE_RESTART = "TRUE")
                    stopApp()
                },
                size = "s", 
                type = 'okOnlyCallback', 
                footer = NULL, 
                easyClose = TRUE
            )            
        } else {
            showUserDialog(
                "Invalid Data Directory", 
                tags$p("Invalid data directory."), 
                tags$p(dir),
                tags$p("Please select a subdirectory named 'data' within a valid MDI installation directory."), 
                type = 'okOnly',
                size = "m"
            )
        }
    }
)

#----------------------------------------------------------------------
# return nothing
#----------------------------------------------------------------------
NULL

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
