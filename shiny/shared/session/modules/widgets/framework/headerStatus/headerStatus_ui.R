#----------------------------------------------------------------------
# static components for feedback on current user and dataDir
# also OAuth2 logout when relevant
#----------------------------------------------------------------------

# module ui function
headerStatusUI <- function(id) {
    
    # initialize namespace
    ns <- NS(id)

    tagList(   
        tags$div(
            class = "header-status",   

            # either:
            #   - OAuth2 user email,
            #   - keyed access user_group name, or
            #   - user's system name on the host machine (when not authenticating)
            textOutput(ns('userDisplayName'), inline = TRUE),

            # command line and unlock buttons, if working in local or remote modes
            if(!serverEnv$IS_SERVER) tagList(
                HTML("&nbsp;&nbsp;"),
                aceEditorLink(ns('aceEditor'), class = "header-status-icon"),
                HTML("&nbsp;&nbsp;"),
                rConsoleLink(ns('rConsole'), class = "header-status-icon"),
                HTML("&nbsp;&nbsp;"),
                commandTerminalLink(ns('commandTerminal'), class = "header-status-icon")
            ) else "",

            # logout button, if REQUIRES_AUTHENTICATION
            if(serverEnv$REQUIRES_AUTHENTICATION && !serverEnv$IS_KEYED) tagList(
                HTML("&nbsp;&nbsp;"),
                actionLink(ns('logout'), label = NULL, icon = icon("sign-out-alt"), class = "header-status-icon")
            ) else "",
            
            # dataDir display, cleanup, and switching
            if(!isAuthorizedUser()){
                ""
            } else { tagList(
                tags$br(),
                textOutput(ns('dataDir'), inline = TRUE), # TODO: enable changing dataDir within running app
                HTML("&nbsp;&nbsp;"),
                if(!serverEnv$IS_SERVER && !is.null(serverConfig$paths)){
                    serverChooseDirIconUI(
                        ns("changeDataDir"), 
                        class = "header-status-dir-icon", 
                        title = "Select an MDI data directory (must end with 'mdi/data')"
                    )                
                } else "",
                HTML("&nbsp;&nbsp;"),
                if(getAuthorizationFlag('serverCleanup')){
                    serverCleanupLink(ns('cleanDataDir'), class = "header-status-icon")           
                } else "",
                HTML("&nbsp;&nbsp;"),
                actionLink(ns('unlockAllRepos'), label = NULL, icon = icon("unlock"), class = "header-status-icon") # nolint            
            )}
        ),         
        tags$div(
            class = "header-status",  
            style = "margin: 0 0.5em;", 
            uiOutput(ns("asyncMonitor"))
        )
    )
}
