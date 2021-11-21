
#----------------------------------------------------------------------
# static components that provide tools to manage a developer's local
# clone of the magc-portal-apps git repository
#----------------------------------------------------------------------
# must never be enabled in server mode, as it allows code modification 
#----------------------------------------------------------------------

# module ui function
gitManagerUI <- function(id, options) {
    
    # initialize namespace
    ns <- NS(id)

    # incorporate options text into templates
    leaderText <- tagList(
        tags$p(HTML("Use the buttons on the left to execute common actions on your local clone of the
               <strong>magc-portal-apps</strong> code repository (they are roughly in the order used
               during development."))
    )

    # return the UI contents
    standardSequentialTabItem("", leaderText,
        fluidRow(
            box(
                width=3,
                title = "Git Actions",
                status = 'primary',
                solidHeader = TRUE,
                #bsButton(ns("pull"),    "Pull from Server", block=TRUE, style="info"), # handled by magc.portal::run now
                bsButton(ns("config"),  "Show Repo Config", block=TRUE, style="success"),
                bsButton(ns("status"),  "Branch Status",    block=TRUE, style="success"),                
                bsButton(ns("branch"),  "List Branches",    block=TRUE, style="primary"),
                bsButton(ns("checkout"),"Switch to Branch", block=TRUE, style="primary"),
                bsButton(ns("stash"),   "Stash Changes",    block=TRUE, style="warning"),
                bsButton(ns("commit"),  "Commit Changes",   block=TRUE, style="warning"),
                bsButton(ns("push"),    "Push To Server",   block=TRUE, style="info")
            ),
            box(
                width=9,
                title = "Git Output",
                status = 'primary',
                solidHeader = TRUE,
                uiOutput(ns('gitOutput'))      
            )
        )
    )    
}

