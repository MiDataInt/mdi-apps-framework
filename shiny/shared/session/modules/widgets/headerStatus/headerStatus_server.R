#----------------------------------------------------------------------
# reactive components for feedback on current user and dataDir
# also Globus logout when relevant
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
headerStatusServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        
# output text
output$user    <- renderText({ headerStatusData$user })
output$dataDir <- renderText({ headerStatusData$dataDir })    

# allow user to log out of Globus
observeEvent(input$logout, {
    req(headerStatusData$user)
    url <- parse_url(globusHelperPages$logout)
    file.remove(sessionFile) # make sure we forget them too
    url$query <- list(
        client_id = globusClient$key, # don't redirect back to app, since it requires logging in again!
        redirect_uri = "https://brcf.medicine.umich.edu/cores/advanced-genomics/",
        redirect_name = "AGC Home Page"        
    )
    runjs(paste0("window.location.href = '", build_url(url),"'"))
})

# allow user to change the dataDir; necessarily reloads the page
observeEvent(input$changeDataDir, {
    showUserDialog(
        "Change the data directory",
        tags$p("Navigate to the folder where the Portal should store/look for its data and analyses and click OK."),
        tags$p("Please note: changing the data directory will reset the web page - you may want to Save Your Work first."),
        tags$p("pending"), # TODO: need a file browser popup, with OK/Cancel
        callback=function(parentInput) NULL,
        size="m"
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

