
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
repo <- gitStatusData$suite$dir
if(is.null(repo)) return()
observers <- list() # for module self-destruction
spinnerSelector <- "#gitManagerSpinner"

#----------------------------------------------------------------------
# remote actions (user must be authorized for these actions via gitCredentials)
#----------------------------------------------------------------------

# update the local clone from the remote repository
observers$pull <- observeEvent(input$pull, {
    gitExpr( quote( git2r::pull(repo) ) )
})

# update the remote repository from the local clone
observers$push <- observeEvent(input$push, {
    gitExpr( quote( git2r::push(repo) ) )
})

#----------------------------------------------------------------------
# local git actions
#----------------------------------------------------------------------

# # git config (to show --local config)
# observers$config <- observeEvent(input$config, {
#     gitExpr( quote( git2r::config(repo) ) )
# })

# git branch (to list all branches)
observers$branch <- observeEvent(input$branch, {
    gitExpr( quote( git2r::branches(repo) ) )
})

# checkout a different branch
observers$checkout <- observeEvent(input$checkout, {

    # provide the most useful feedback to user if checkout is impossible
    if(pendingChangesRefusal(
        'You cannot switch branches because you have uncommitted code changes that
        would be overwritten.'
    )) return(NULL)

    # initialize dialog
    branchNames <- listGitBranchNames(localOnly = FALSE)
    remoteBranches <- grepl('remotes/', branchNames)    
    localBranchNames  <- branchNames[!remoteBranches]
    remoteBranchNames <- branchNames[ remoteBranches]
    localBranchId  <- ns('localBranch')
    remoteBranchId <- ns('remoteBranch')
    mainBranchTag <- ns('mainTag')
    newBranchId <- ns('newBranch')
    nullOption <- '---'
    
    # show branch select/create dialog with callback
    showUserDialog(
        'Select / Create a Branch',
        tags$p("The first non-empty branch name from top to bottom will become the working branch."),
        tags$p("Any new branch will be created using the current branch as parent."),
        selectInput(localBranchId,  'Local Branches',  c(nullOption, localBranchNames)),
        selectInput(remoteBranchId, 'Remote Branches', c(nullOption, remoteBranchNames)),
        textInput(mainBranchTag, 'Version Tag (e.g. v1.0.2 - include the v!)', ''),
        textInput(newBranchId, 'New Branch Name', ''),
        callback = function(parentInput){
            branchId <- if(parentInput[[localBranchId]]  != nullOption) localBranchId
                   else if(parentInput[[remoteBranchId]] != nullOption) remoteBranchId
                   else if(trimws(parentInput[[mainBranchTag]]) != '') mainBranchTag
                   else if(trimws(parentInput[[newBranchId]]) != '') newBranchId
                   else NULL
            if(!is.null(branchId)) {
                branch <- trimws(parentInput[[branchId]])
                reportProgress(paste("switching to git branch:", branch))
                gitExpr( checkoutGitBranch(branch, create = branchId == newBranchId) )
                isolate(sessionEnv$invalidateGitBranch( sessionEnv$invalidateGitBranch() + 1 ))
            } else {
                stop('please select or type a branch name')
            }
        }
    )    
})

# show the status on the current branch
observers$status <- observeEvent(input$status, {
    gitExpr( quote( git2r::status(repo) ) )
})
getStatus <- function(){
    dprint(git2r::status(repo))
# List of 3
#  $ staged   : Named list()
#  $ unstaged : Named list()
#  $ untracked: Named list()
#  - attr(*, "class")= chr "git_status    
}

# stash (i.e. set aside, save) current code changes
observers$stash <- observeEvent(input$stash, {
    getStashCommitMessage(
        'Stash',
        'Enter Stash Message',
        'Stashing means your code changes will be saved and set aside. Your working
        files will be rolled back to the previous committed state and NOT committed.',
        stashAllGitBranch
    )
})

# add (i.e. stage) and commit all current code changes
# finer, more granular control requires use of an external git interface
observers$commit <- observeEvent(input$commit, {
    getStashCommitMessage(
        'Commit',
        'Enter Commit Message',
        'ALL active code changes on your current branch will be staged AND committed
        (for more granular control, please use a different git utility).',
        commitAllGitBranch
    )
})

#----------------------------------------------------------------------
# shared dialog for stash/commit message input and action refusals
#----------------------------------------------------------------------
getStashCommitMessage <- function(type, title, subtitle, gitFn){
    
    # check for something to do
    pendingChanges <- !isGitCommitted()
    if(!pendingChanges) return(
        showUserDialog(
            'No changes are pending',
            tags$p('Nothing to do.'),           
            type = 'okOnly'
        )
    )
    
    # then do it
    messageId <- 'message'
    showUserDialog(
        title,
        tags$p(subtitle),
        tags$p("Please enter a short descriptive message for these code changes (e.g. 'fixed bug in xxx'):"),
        textInput(messageId, paste(type, 'Message'), ''),        
        callback = function(parentInput){
            message <- trimws(parentInput[[messageId]])
            if(message == '') stop('missing message (required)')
            else if(nchar(message) < 8) stop('message must be at least 8 characters')
            else gitExpr( gitFn(message) )
        }
    )    
}
confirmGitRemoteAction <- function(type, Type, gitFn, message){
    
    # give useful feedback when we refuse the action
    if(pendingChangesRefusal(
        paste('It is not recommended to', type, 'code when you have pending local changes
        (use other git utilities if you require finer control).')
    )) return(NULL)
    
    # get permission for valid remote actions
    showUserDialog(
        paste('Confirm', Type),
        tags$p(message),
        tags$p("Proceed?"),   
        callback = function(parentInput){
            gitExpr( gitFn() )
        }
    )         
}

#----------------------------------------------------------------------
# show git2r output (not the function result) in UI
#----------------------------------------------------------------------
gitExpr <- reactiveVal(NULL)
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
        style = "max-height: 800px; overflow: auto;",
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
