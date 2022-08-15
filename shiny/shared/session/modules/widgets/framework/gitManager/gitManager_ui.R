
#----------------------------------------------------------------------
# static components that apply git functions to the running app
#----------------------------------------------------------------------

# module ui function
gitManagerUI <- function(id) {
    ns <- NS(id)
    fluidRow(
        box(
            width = 3,
            title = "Actions",
            status = 'primary',
            solidHeader = TRUE,
            bsButton(ns("pull"),     "Pull from Server", block = TRUE, style = "info"), 
            # bsButton(ns("config"),   "Show Repo Config", block = TRUE, style = "success"),
            bsButton(ns("status"),   "Branch Status",    block = TRUE, style = "success"),                
            bsButton(ns("branch"),   "List Branches",    block = TRUE, style = "primary"),
            bsButton(ns("checkout"), "Switch to Branch", block = TRUE, style = "primary"),
            bsButton(ns("stash"),    "Stash Changes",    block = TRUE, style = "warning"),
            bsButton(ns("commit"),   "Commit Changes",   block = TRUE, style = "warning"),
            bsButton(ns("push"),     "Push To Server",   block = TRUE, style = "info", disabled = TRUE)
        ),
        box(
            width = 9,
            title = "Output",
            status = 'primary',
            solidHeader = TRUE,
            uiOutput(ns('output'))      
        )
    )   
}
