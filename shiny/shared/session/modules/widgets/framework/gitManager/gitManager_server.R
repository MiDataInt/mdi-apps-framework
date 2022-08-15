#----------------------------------------------------------------------
# reactive components that apply git functions to the running app
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
gitManagerServer <- function(id, parentId, options) {
    moduleServer(id, function(input, output, session) {
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize the module
#----------------------------------------------------------------------
module <- "gitManager"
observers <- list() # for module self-destruction
spinnerSelector <- "#gitManagerSpinner"
blur <- function(id) runjs( paste0("document.getElementById('", session$ns(id), "').blur();") )
origin <- "origin"
buffer <- NULL # for the status table
repo <- reactiveVal(NULL) # either framework or suite
status <- reactiveVal(list(type = "app")) # for the currently selected repo
pendingAction <- reactiveVal(NULL) # when stash or commit have been checked
gitExpr <- reactiveVal(NULL) # for passing to the execution and display function

#----------------------------------------------------------------------
# repository and app status table
#----------------------------------------------------------------------
getRepoTable <- function(type, repo) data.table(
    "Type" = type,
    "Name" =  repo$name,
    "Fork" = if(isDeveloperFork(repo$dir)) "developer-fork" else "definitive",
    "Head / Version" = getGitHeadDisplay(repo$head),
    "Commit" =  getShortGitCommit(repo$head$sha)
)
output$statusTable <- renderDT(
    {
        dt <- getRepoTable("framework", gitFrameworkStatus)
        if(!is.null(gitStatusData$suite$name)) dt <- rbind(
            dt,
            getRepoTable("suite", gitStatusData$suite),
            data.table(
                "Type" = "app",
                "Name" =  gitStatusData$app$name,
                "Fork" = "",
                "Head / Version" = gitStatusData$app$version,
                "Commit" = ""
            )
        )
        buffer <<- dt
    },
    options = list( # just a barebones table
        searching = FALSE,
        paging = FALSE,
        bInfo = FALSE
    ),
    class = "display table-compact-4",
    escape = FALSE, 
    selection = "single", 
    editable = FALSE,
    rownames = FALSE,
    server = FALSE
)

# update the extended status of a clicked repository
repoObserver <- rowSelectionObserver("statusTable", input)
setRepoStatus <- function(showStatus = TRUE){
    show(selector = spinnerSelector) 
    dir <- repo()$dir

    # collect various bits of status and other metadata on the selected repo
    git2r::fetch(dir, origin)
    x <- list(
        type = "repository",
        local = git2r::status(dir)
    )
    x$unstaged <- length(x$local$unstaged) + length(x$local$untracked) > 0     
    x$head <- git2r::repository_head(dir)
    x$localBranches  <- names(git2r::branches(dir, "local"))
    x$remoteBranches <- names(git2r::branches(dir, "remote"))
    x$remoteBranchesLong <- x$remoteBranches
    x$remoteBranches <- gsub("origin/", "", x$remoteBranches)
    x$remoteBranches <- x$remoteBranches[
        x$remoteBranches != "HEAD" &
        !(x$remoteBranches %in% x$localBranches)
    ]
    x$detached <- git2r::is_detached(dir) 
    if(!x$detached){
        x$branch <- x$head$name
        remoteBranchLong <- paste("origin", x$branch, sep = "/")
        if(remoteBranchLong %in% x$remoteBranchesLong){
            ahead_behind <- git2r::ahead_behind(
                x$head, 
                git2r::revparse_single(dir, remoteBranchLong)
            )
            x$ahead  <- ahead_behind[1]
            x$behind <- ahead_behind[2]  
        } else {
            x$ahead  <- 0
            x$behind <- 0
        }
    } else {
        x$ahead  <- 0
        x$behind <- 0
    }
    x$isAhead  <- x$ahead  > 0  
    x$isBehind <- x$behind > 0  

    # update the UI based on the repo's status to prevent unwarranted action sequences
    updateButton(session, session$ns("pull"), # enable pull if there is something to pull   
                 style = if(x$isBehind) "primary" else "default",   
                 disabled = !x$isBehind)
    updateButton(session, session$ns("stash"), # enable stash and commit if there are pending changes     
                 style = if(x$unstaged) "danger" else "default",   
                 disabled = !x$unstaged)
    updateButton(session, session$ns("commit"),   
                 style = if(x$unstaged) "success" else "default",   
                 disabled = !x$unstaged)
    updateButton(session, session$ns("push"), # enable push if there is something to pull    
                 style = if(x$isAhead) "primary" else "default",   
                 disabled = !x$isAhead)
    updateButton(session, session$ns("checkout"), # enable stash and commit if there are NOT pending changes   
                 style = if(!x$unstaged) "primary" else "default",   
                 disabled = x$unstaged)          
    updateSelectInput(session, "references", choices = c(
        x$localBranches, # a list of all relevant checkout targets
        x$remoteBranches,        
        names(repo()$versions)
    ), selected = getGitHeadDisplay(repo()$head))
    hide(selector = spinnerSelector) 
    status(x)
    if(showStatus) invalidateStatus( invalidateStatus() + 1 )
}

# show the repo status in the UI
invalidateStatus <- reactiveVal(0)
observers$statusUpdate <- observeEvent(invalidateStatus(), {
    status <- status()
    gitExpr(switch(
        status$type,
        app = quote(""),
        quote({
            status <- status()
            if(status$detached) cat("head is detached\n")
            else {
                cat(paste("on branch", status$branch, "\n"))
                cat(paste(status$ahead, "commits ahead and", status$behind, "commits behind origin", "\n"))
            }
            print(status$local) 
        })
    ))
}, ignoreInit = TRUE)
observers$repoSelected <- observeEvent(repoObserver(), {

    # parse the row
    rowI <- repoObserver()
    isRowSelected <- !is.na(rowI)
    type <- if(isRowSelected) buffer[rowI, Type] else "app"
    isRepoSelected <- type != "app"

    # set the associated repository (if any)
    toggle(selector = ".isRepoSelected", condition = isRepoSelected)
    repo(switch(
        type,
        framework = gitFrameworkStatus,
        suite = gitStatusData$suite,
        NULL
    )) 

    # reset the repo status and UI
    hide("messagePanel")
    pendingAction(NULL)
    if(isRepoSelected) setRepoStatus(TRUE) else {
        status(list(type = "app"))
        invalidateStatus( invalidateStatus() + 1 )
    }
}, ignoreInit = TRUE)

# respond to the status button click
observers$statusButton <- observeEvent(input$status, {
    blur('status')
    setRepoStatus(TRUE)
}, ignoreInit = TRUE)

#----------------------------------------------------------------------
# remote push/pull actions (user must be authorized via gitCredentials and thus GITHUB_PAT)
#----------------------------------------------------------------------

# update the local clone from the remote repository
observers$pull <- observeEvent(input$pull, {
    blur('pull')
    gitExpr( quote({ git2r::pull(repo()$dir, credentials = git2r::cred_token()) }) )
}, ignoreInit = TRUE)

# update the remote repository from the local clone
observers$push <- observeEvent(input$push, {
    blur('push')
    gitExpr( quote({ 
        git2r::push(repo()$dir, credentials = git2r::cred_token()) 
        setRepoStatus(TRUE) # push itself is silent
    }) )
}, ignoreInit = TRUE)

#----------------------------------------------------------------------
# local change commit/stash actions
#----------------------------------------------------------------------

# confirm stash and commit actions
output$confirm <- renderText({ 
    req(pendingAction())
    req(pendingAction()$type == "confirm")
    pendingAction()$prompt
})
output$warn <- renderText({
    req(pendingAction())
    req(pendingAction()$type == "warn") 
    pendingAction()$prompt
})
observers$message <- observeEvent(pendingAction(), {
    toggle(
        'messagePanel', 
        condition = !is.null(pendingAction()) && pendingAction()$message
    )
})

# prompt to collect the stash/commit message
doMessageAction <- function(action, type, prompt, expr){
    pending <- pendingAction()
    if(is.null(pending) || pending$action != action){
        pendingAction(list(
            action = action,
            type = type,
            message = TRUE,
            prompt = prompt
        ))
    } else {
        req(input$message)
        pendingAction(NULL)
        hide("messagePanel")
        gitExpr(expr)
    }
}

# stash (i.e. set aside, save) current code changes
observers$stash <- observeEvent(input$stash, {
    blur('stash')
    doMessageAction(
        "stash", 
        "warn", 
        "Enter a message and click 'Stash All' again to confirm and stash all changes.", 
        quote({
            git2r::stash(repo()$dir, input$message, untracked = TRUE)
            setRepoStatus(TRUE) # stash itself is silent
        })
    )
}, ignoreInit = TRUE)

# add (i.e. stage) and commit all current code changes
# finer, more granular control requires use of an external git interface
observers$commit <- observeEvent(input$commit, {
    blur('commit')
    doMessageAction(
        "commit", 
        "confirm", 
        "Enter a message and click 'Commit All' again to confirm and commit all changes.", 
        quote({
            git2r::add(repo()$dir, ".")
            git2r::commit(repo()$dir, input$message)
        })
    )
}, ignoreInit = TRUE)

#----------------------------------------------------------------------
# local branch actions
#----------------------------------------------------------------------

# checkout a different branch
observers$checkout <- observeEvent(input$checkout, {
    blur('checkout')

    # parse and checkout the repo as requested
    create <- if(is.null(input$create)) "" else trimws(input$create)    
    if(create == ""){
        target <- if(is.null(input$references)) "" else trimws(input$references)
        isCreate <- FALSE
    } else {
        target <- create
        isCreate <- TRUE
    }
    req(target)
    if(target == getGitHeadDisplay(repo()$head)) return()
    git2r::checkout(repo()$dir, target, create = isCreate)

    # restart the server with checkout suppression in mdi::run()
    showUserDialog(
        "Server Restart Required", 
        tags$p("Please reload a fresh web page to start a new session once the server restarts."),
        callback = function(...) {
            Sys.setenv(MDI_FORCE_RESTART = "TRUE")
            Sys.setenv(MDI_SUPPRESS_CHECKOUT = "TRUE")
            stopApp()
        },
        size = "s", 
        type = 'okOnlyCallback', 
        footer = NULL, 
        easyClose = TRUE
    )    
}, ignoreInit = TRUE)

#----------------------------------------------------------------------
# show git2r output in UI
#----------------------------------------------------------------------
output$output <- renderUI({
    expr <- gitExpr()    
    req(expr)
    show(selector = spinnerSelector) 
    output <- tryCatch(
        capture.output(eval(expr)),
        warning = function(w) w, 
        error = function(e) e
    ) 
    hide(selector = spinnerSelector)  
    tags$pre(
        style = "max-height: 400px; overflow: auto;",
        paste(collapse = "\n", output)
    )
})

#----------------------------------------------------------------------
# set return value
#----------------------------------------------------------------------
list(
    observers = observers, # for use by destroyModuleObservers
    onDestroy = function() {
        list(  # return the module's cached state object
        )               
    }
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
