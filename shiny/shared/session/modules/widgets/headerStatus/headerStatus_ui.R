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
        
        # either:
        #   - OAuth2 user email,
        #   - keyed access user_group name, or
        #   - user's system name on the host machine (when not authenticating)
        textOutput(ns('userDisplayName'), inline = TRUE),
        
        # logout button, if REQUIRES_AUTHENTICATION
        if(serverEnv$REQUIRES_AUTHENTICATION && !serverEnv$IS_KEYED) tagList(
            HTML("&nbsp;&nbsp;"),
            actionLink(ns('logout'), label = NULL, icon = icon("sign-out-alt"))
        ) else "",
        
        # dataDir display and switching
        # if(serverEnv$IS_SERVER){
        #     ""
        # } else { tagList(
            tags$br(),
            textOutput(ns('dataDir'), inline = TRUE), # TODO: enable changing dataDir within running app
            HTML("&nbsp;&nbsp;"),
            actionLink(ns('changeDataDir'), label = NULL, icon = icon("folder"))
        # )}
    )
}
