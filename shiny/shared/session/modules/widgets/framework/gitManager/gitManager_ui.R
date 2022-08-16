#----------------------------------------------------------------------
# static components that apply git functions to the running app
#----------------------------------------------------------------------
gitManagerUI <- function(id) {
    ns <- NS(id)
    suppressDeveloper <- !serverEnv$IS_DEVELOPER
    tagList(

        # table of up to three items: framework, suite, app
        fluidRow(
            style = "margin-bottom: 15px;",
            column(
                width = 10,
                DTOutput(ns("statusTable"))
            )
        ),

        # git action buttons (except for checkout, see below)
        if(suppressDeveloper) "" else fluidRow(
            id = ns("actions"),
            class = "isRepoSelected",
            style = "display: none; margin-bottom: 15px;",
            column(
                width = 2,
                bsButton(ns("status"),    "Status",         
                         block = TRUE, style = "primary", disabled = FALSE)
            ),
            column(
                width = 2,
                bsButton(ns("pull"),    "Pull",         
                         block = TRUE, style = "default", disabled = TRUE)
            ),
            column(
                width = 2,
                bsButton(ns("stash"),   "Stash All",    
                         block = TRUE, style = "default", disabled = TRUE)
            ),
            column(
                width = 2,
                bsButton(ns("commit"),  "Commit All",   
                         block = TRUE, style = "default", disabled = TRUE)
            ),
            column(
                width = 2,
                bsButton(ns("push"),    "Push",         
                         block = TRUE, style = "default", disabled = TRUE)
            ),
        ),

        # git checkout action, to local, remote or new branch, or a version tag
        fluidRow(
            id = ns("checkoutPanel"),
            class = "isRepoSelected",
            style = "display: none",
            column(
                width = 2,
                bsButton(ns("checkout"),    "Checkout",         
                         block = TRUE, style = "default", disabled = TRUE)
            ),
            column(
                width = 4,
                selectInput(ns("references"), label = NULL, choices = list(), width = "100%")
            ),
            if(suppressDeveloper) "" else column(
                width = 4,
                textInput(ns("create"), label = NULL, placeholder = "enter a new branch name")
            )
        ),

        # confirmation and messages for stash and commit actions
        if(suppressDeveloper) "" else fluidRow(
            column(
                width = 12,
                style = "color: rgb(0, 150, 0);",
                textOutput(ns("confirm"))
            ),
            column(
                width = 12,
                style = "color: rgb(200, 0, 0);",
                textOutput(ns("warn"))
            ),
            column(
                id = ns("messagePanel"),
                width = 8,
                style = "margin-top: 15px; display: none;",
                textInput(ns('message'), label = "Message", width = "100%") 
            ),

            # on-screen display of the output of the last git command executed
            column(
                width = 12,
                style = "margin-top: 15px;",
                uiOutput(ns('output')) 
            )
        ) 
    )              
}
