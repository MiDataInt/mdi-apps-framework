#----------------------------------------------------------------------
# static components for feedback on current user and dataDir
# also OAuth2 logout when relevant
#----------------------------------------------------------------------

# module ui function
headerStatusUI <- function(id) {
    
    # initialize namespace
    ns <- NS(id)

    tags$div(
        class = "header-status",
        
        # either authenticated user email or the user's system name on the host machine
        textOutput(ns('user'), inline = TRUE),
        
        # logout button, if REQUIRES_AUTHENTICATION
        if(serverEnv$REQUIRES_AUTHENTICATION) tagList(
            HTML("&nbsp;&nbsp;"),
            actionLink(ns('logout'), label = NULL, icon = icon("sign-out"))
        ) else "",
        
        # dataDir display and switching, if not IS_SERVER
        if(serverEnv$IS_SERVER){
            ""
        } else { tagList(
            tags$br(),
            textOutput(ns('dataDir'), inline = TRUE), # TODO: enable changing dataDir within running app
            HTML("&nbsp;&nbsp;"),
            actionLink(ns('changeDataDir'), label = NULL, icon = icon("folder"))
        )}
    )
}
