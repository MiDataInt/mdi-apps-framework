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
    headerStatusData$userDisplayName 
})
output$dataDir <- renderText({ 
    req(headerStatusData$userDisplayName)
    headerStatusData$dataDir 
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

# allow user to change the dataDir; necessarily reloads the page
observeEvent(input$changeDataDir, {
    req(headerStatusData$userDisplayName)
    req(headerStatusData$dataDir)
    showUserDialog(
        "Change the data directory",
        tags$p("Navigate to the folder where apps should store/look for data and analyses and click OK."),
        tags$p("Please note: changing the data directory will reset the web page - you may want to Save Your Work first."), # nolint
        tags$p("pending"), # TODO: need a file browser popup, with OK/Cancel
        callback = function(parentInput) NULL,
        size = "m"
    ) 
})

#----------------------------------------------------------------------
# return nothing
#----------------------------------------------------------------------
NULL

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
