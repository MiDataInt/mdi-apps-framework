
#----------------------------------------------------------------------
# reactive components that provide tools to manage a developer's local
# clone of the magc-portal-apps git repository
#----------------------------------------------------------------------
# must never be enabled in server mode, as it allows code modification 
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
gitManagerServer <- function(id, parentId, options) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        parentNs <- NS(parentId)
        module <- 'gitManager' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# remote actions (user most already be authorized for these actions on their system)
#----------------------------------------------------------------------

# update the local clone from the remote repository
observeEvent(input$pull, {
    confirmGitRemoteAction(
        'pull',
        'Pull',
        pullRepositoryBranch,
        'The pull action will update your local clone from the remote repository.'
    )
})

# update the remote repository from the local clone
# action always sends the current branch plus main and develop (which may have been updated from UMAGC)
observeEvent(input$push, {
    confirmGitRemoteAction(
        'push',
        'Push',
        pushRepositoryBranches,
        'The push action will update your remote repository from your local clone.'
    )
})

#----------------------------------------------------------------------
# local git actions
#----------------------------------------------------------------------

# git config (to show --local config)
observeEvent(input$config, {
    gitOutput( showGitRepoConfig() )
})

# git branch (to list all branches)
observeEvent(input$branch, {
    gitOutput( listGitBranches() )
})

# checkout a different branch
observeEvent(input$checkout, {

    # provide the most useful feedback to user if checkout is impossible
    if(pendingChangesRefusal(
        'You cannot switch branches because you have uncommitted code changes that
        would be overwritten.'
    )) return(NULL)

    # initialize dialog
    branchNames <- listGitBranchNames(localOnly=FALSE)
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
        textInput(mainBranchTag,'Version Tag (e.g. v1.0.2 - include the v!)',''),
        textInput(newBranchId,'New Branch Name',''),
        callback = function(parentInput){
            branchId <- if(parentInput[[localBranchId]]  != nullOption) localBranchId
                   else if(parentInput[[remoteBranchId]] != nullOption) remoteBranchId
                   else if(trimws(parentInput[[mainBranchTag]]) != '') mainBranchTag
                   else if(trimws(parentInput[[newBranchId]]) != '') newBranchId
                   else NULL
            if(!is.null(branchId)) {
                branch <- trimws(parentInput[[branchId]])
                reportProgress(paste("switching to git branch:", branch))
                gitOutput( checkoutGitBranch(branch, create = branchId==newBranchId) )
                isolate(sessionEnv$invalidateGitBranch( sessionEnv$invalidateGitBranch() + 1 ))
            } else {
                stop('please select or type a branch name')
            }
        }
    )    
})

# show the status on the current branch
observeEvent(input$status, {
    gitOutput( getGitBranchStatus() )
})

# stash (i.e. set aside, save) current code changes
observeEvent(input$stash, {
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
observeEvent(input$commit, {
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
            else gitOutput( gitFn(message) )
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
            gitOutput( gitFn() )
        }
    )         
}

#----------------------------------------------------------------------
# show git output in UI for executed commands
#----------------------------------------------------------------------
gitOutput <- reactiveVal(NULL)
output$gitOutput <- renderUI({
    expr <- gitOutput()
    req(expr)
    startSpinner(session, 'output$gitOutput')
    git <- eval(expr)
    if(!git$success) {
        reportProgress('output$gitOutput failed:')
        print(git$results)
    }
    stopSpinner(session)     
    tags$pre(
        style="max-height: 800px; overflow: auto;",
        paste(collapse="\n", git$results)
    )

})

#----------------------------------------------------------------------
# set return value
#----------------------------------------------------------------------
NULL

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------

