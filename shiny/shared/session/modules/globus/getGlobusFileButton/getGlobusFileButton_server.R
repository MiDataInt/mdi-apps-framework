
#----------------------------------------------------------------------
# reactive components for a button to link user to Globus for file selection
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
getGlobusFileButtonServer <- function(id, handler, folderlimit = '0', filelimit = '1', appName=NULL) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id)

# activate the button
observeEvent(input$button, {
    state <- list(
        loginHandler = 'redirectToGlobusFileManager',
        handler = handler,
        folderlimit = folderlimit,
        filelimit = filelimit,
        bookmark = if(is.null(appName)) NULL else getBookmarkJson(),
        allowedFileTypes = getAllowedSourceFileTypes(appName)
    )
    if(is.null(globusUserData$user)){ # get user login if not already logged in
        runjs(paste0("window.location.href = '", getGlobusRedirectUrl(sessionKey, state), "'"))
    } else {
        redirectToGlobusFileManager(sessionKey, state)
    }
})

#observeEvent(input$button, {
#    showUserDialog(
#        "Select File to Import",
#        fluidRow(
#            column(width=6,
#                actionLink('myGlobusEndpoints', 'My Endpoints'),
#                actionLink('myGlobusBookmarks', 'My Bookmarks'),
#                tags$div(uiOutput('myGlobusEndpointSelector'))
#            ),
#            column(width=6,
#                tags$p('current path goes here'),
#                tags$div('folder content go here')
#            ),
#            style = "height: 80%; max-height: 500px;"
#        ),
#        callback=function(parentInput) NULL,
#        size="l",
#        type='okCancel',
#        easyClose=FALSE
#    )
#})

#----------------------------------------------------------------------
# return nothing
#----------------------------------------------------------------------
NULL

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------

