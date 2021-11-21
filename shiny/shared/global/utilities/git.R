#----------------------------------------------------------------------
# manipulate an MDI git repository
#----------------------------------------------------------------------
# this version uses system calls to git, instead of git2r
# system git gives nicer output (and git2r had some less-than-optimal behavior)
#----------------------------------------------------------------------
# some commands require that user be known to git to associate with the commit
# mdi::develop() ensures that user info is present in --local (i.e., in git repo)
# that info can be changed by developer as needed with:
#       git config --local user.name  "xx xx"
#       git config --local user.email "xx@xx.xx"
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# the core function that makes calls to git on the system
#----------------------------------------------------------------------
systemGit <- function(args){
    if(!serverEnv$IS_DEVELOPER) return(list(success = FALSE, results = "denied"))
    wdTmp <- setwd(serverEnv$MAGC_PORTAL_APPS_DIR) # just in case someone has changed wd
    expr <- quote(system2('git', args = args, stdout = TRUE, stderr = TRUE))
    success <- TRUE
    results <- tryCatch({
        eval(expr)
    }, warning = function(w){
        success <<- FALSE
        eval(expr) # if git itself fails, the git error is reported as output with an R warning
    }, error = function(e){
        success <<- FALSE
        e$message # if the call to git fails on the system, it is reported as an R error
    })
    setwd(wdTmp) # restore any prior working directory (although changing it is NOT recommended)
    list(
        success = success,
        results = results
    )    
}

#----------------------------------------------------------------------
# top-level action called by server.R to initialize a branch on app change in developer mode
#----------------------------------------------------------------------
switchGitBranch <- function(appName, session, sessionEnv, CONSTANTS){
    
    # determine the target branch
    BRANCH <- appName 
    currentBranch <- getCurrentGitBranch()
    if(BRANCH != currentBranch && # FALSE if already on the branch of interest (no change is needed)
       currentBranch == CONSTANTS$defaultEditBranch){ # do not force developer off of some other preferred branch

        # assess whether branch change is possible
        reportProgress(paste('switching to branch:', BRANCH))
        if(sessionEnv$pendingChangesRefusal(
            message = tagList(
                tags$p('You cannot switch git branches because you have uncommitted code changes that would be overwritten.'), # nolint
                tags$p(HTML(paste('You may continue working, but BE AWARE that you are still on the', tags$strong(currentBranch), 'branch!'))) # nolint
            ),
            suffix = 'to enable branch switching'
        )){
            reportProgress('branch switch failed due to uncommitted changes')
            stopSpinner(session)
            return(FALSE) # refuse to act on the app selection; UI remains unchanged
        }
        
        # if possible, execute the switch via git checkout
        checkoutGitBranch(CONSTANTS$developerBranch, eval = TRUE) # ensure that all new branches come off of develop
        checkout <- checkoutGitBranch(BRANCH, create = TRUE, eval = TRUE)
        
        # finally, make sure new branch is up to date with interim changes on develop branch
        if(checkout$success) updateDevelopIntoBranch(BRANCH, CONSTANTS)
        checkout$success
    } else {
        TRUE        
    }
}
# this function updates an app branch with any commits into UMAGC:develop
# that have occurred since our app branch was intially declared
gitMergeInProgress <- NULL
updateDevelopIntoBranch <- function(BRANCH, CONSTANTS){
    reportProgress(paste('updating branch from develop:', BRANCH))

    # merge succeeds silently if:
    #   already up to date, no merge required (i.e. no interim changes on develop)
    #   BRANCH could be fast-forwarded to incorporate all interim changes on develop (i.e. no edits on BRANCH)
    #   there were no merge conflicts between the diverged branches
    merge <- mergeFromBranch(CONSTANTS$developerBranch, eval = TRUE)
    if(!merge$success) {
        gitMergeInProgress <<- list(
            recipientBranch = BRANCH,
            sourceBranch = CONSTANTS$developerBranch,
            BRANCH = BRANCH,
            CONSTANTS = CONSTANTS,
            continue = FALSE
        )
        showGitMergeConflicts()
    }
}
# if merge conflicts exist, show them to user and demand either --abort or --continue to restore a proper state
showGitMergeConflicts <- function(){
    conflictFiles <- systemGit(c('diff', '--name-only', '--diff-filter=U'))
    files <- conflictFiles$results
    files <- files[!grepl(' ', files)] # discard some warning messages like CRLF in the systemGit output 
    showUserDialog(    
        'Merge conflicts must be resolved',
        if(gitMergeInProgress$continue) tags$p('Merge conflicts are still present.') else "",
        tags$p(HTML(paste(
            'Other developers made code changes that must be merged into your repository fork.',
            paste('However, conflicts between branches',
                  tags$strong(gitMergeInProgress$recipientBranch),
                  'and',
                  tags$strong(gitMergeInProgress$sourceBranch),
                  'are preventing merging from being completed.'
            )
        ))),    
        tags$p('These files are currently in a conflicted state:'),
        tags$p(HTML(paste(files, '<br>')), style = "color: blue;"),
        tags$p(paste(
            'Please manually edit all files to resolve the conflicts and then Continue.',
            'Abort to stop merging, but you eventually have to complete the merge.'
        )),
        size = 'm',
        easyClose = FALSE,
        type = 'custom',
        footer = tagList( # an action that require input and/or confirmation
            actionButton("abortGitMerge", "Abort"),
            actionButton("continueGitMerge", "Continue")
        )
    )  
}
observeEvent(input$abortGitMerge, {
    systemGit(c('merge', '--abort'))
    removeModal()
})
observeEvent(input$continueGitMerge, {
    message <- paste(gitMergeInProgress$sourceBranch, gitMergeInProgress$recipientBranch, sep = " into ")
    message <- paste('merge conflicts resolved by user from branch', message)
    commitAllGitBranch(message, eval = TRUE)
    removeModal()
    gitMergeInProgress$continue <<- TRUE
    updateDevelopIntoBranch(gitMergeInProgress$BRANCH, gitMergeInProgress$CONSTANTS)
})

#----------------------------------------------------------------------
# these functions return values (by calling expressions below)
#----------------------------------------------------------------------

# get the name of the current repository, i.e. the fork of magc-portal-apps
getCurrentGitRepo <- function(){
    x <- systemGit(c('config', '--get', 'remote.origin.url'))
    if(x$success) {
        x <- sub('https://github.com/', '', x$results)
        x <- sub('/magc-portal-apps.git', '', x)
        x
    } else 'ERROR'
}

# get the name of the current branch, i.e. location of HEAD
# returns a simple character value
getCurrentGitBranch <- function(){
    git <- listGitBranches(localOnly = TRUE, eval = TRUE)
    if(!git$success) '**ERROR**'
    else {
        x <- git$results[which(startsWith(git$results, '*'))]
        strsplit(x, ' ')[[1]][2]
    }   
}

# get the names of all branches
listGitBranchNames <- function(localOnly = TRUE){
    git <- listGitBranches(localOnly = localOnly, eval = TRUE)
    if(!git$success) return('**ERROR**')
    else {
        sapply(strsplit(git$results, ' '), function(v) v[length(v)])
    }    
}

# determine whether the current branch is fully committed (return TRUE if so)
isGitCommitted <- function(){
    length(getGitBranchStatus(short = TRUE, eval = TRUE)$results) == 1
}

#----------------------------------------------------------------------
# these functions return unevaluated expressions (unless requested to evaluate)
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# remote actions (user most already be authorized for these actions on their system)
#----------------------------------------------------------------------

# update the local clone from the remote repository
pullRepositoryBranch <- function(eval = FALSE){
    reportProgress('pull', 'git')
    expr <- quote(
        systemGit(c('pull'))
    )
    if(eval) eval(expr) else expr
}

# update the remote repository from the local clone
# action always sends the current branch plus main and develop (which may have been updated from UMAGC)
pushRepositoryBranches <- function(eval = FALSE){
    reportProgress('push', 'git')
    branches <- c(
        CONSTANTS$mainBranch,
        CONSTANTS$developerBranch,            
        getCurrentGitBranch() # last to ensure that we don't change branches by this action
    )
    for(i in 1:3){
        branch <- branches[i]
        systemGit(c('checkout', branch))
        if(i == 3){ # make sure git knows the remote for app branches and prepar for output display in UI
            expr <- substitute(
                systemGit(c('push', '--set-upstream', CONSTANTS$originRemote, branch)),
                list(branch = branch)
            )           
        } else { # main and develop are pushed silently
            systemGit(c('push'))
        }
    } 
    if(eval) eval(expr) else expr # thus the final action reflects the current app branch
}

#----------------------------------------------------------------------
# local actions
#----------------------------------------------------------------------

# git config (to show --local config)
showGitRepoConfig <- function(eval = FALSE){
    reportProgress('config', 'git')
    expr <- quote(
        systemGit(c('config', '--local', '--list'))
    )
    if(eval) eval(expr) else expr
}

# get branches, with a label on HEAD
listGitBranches <- function(localOnly = TRUE, eval = FALSE){
    reportProgress('branch', 'git')
    expr <- substitute(
        systemGit(c('branch', if(localOnly) '' else '-a')),
        list(localOnly = localOnly)
    )
    if(eval) eval(expr) else expr
}

# show the status on the current branch
getGitBranchStatus <- function(short = FALSE, eval = FALSE){
    reportProgress('status', 'git')
    expr <- substitute(
        systemGit(c('status', '-b', if(short) '-s' else '')), # if short format, first line is branch information
        list(short = short)
    )
    if(eval) eval(expr) else expr
}

# change to a specific branch
checkoutGitBranch <- function(branch, create = FALSE, eval = FALSE){
    reportProgress('checkout', 'git')
    expr <- substitute(
        {
            create_ <- if(create) !(branch %in% listGitBranchNames()) else FALSE
            systemGit(c('checkout', if(create_) '-b' else '', branch))
        }, 
        list(branch = branch, create = create)
    )
    if(eval) eval(expr) else expr
}
pendingChangesRefusal <- function(message, suffix = 'first'){
    pendingChanges <- !isGitCommitted()
    if(pendingChanges){
        showUserDialog(
            type = 'okOnly',
            'Changes are pending',
            tags$p(message),
            tags$p(HTML(paste0('Please use <strong>Stash Changes</strong> or <strong>Commit Changes</strong> ', suffix, '.'))) # nolint
        )
    }
    pendingChanges # i.e. return true if we refused the action based on pending changes
}

# stash all code changes (required to change branches, etc.)
stashAllGitBranch <- function(message, eval = FALSE){
    reportProgress('stash', 'git')
    expr <- substitute(
        {
            msg <- message
            systemGit(c('stash', '-a', '-m', shQuote(msg)))
        },
        list(message = message)
    )
    if(eval) eval(expr) else expr
}

# stage and commit all local code changes in one step
# does not support granular control
commitAllGitBranch <- function(message, eval = FALSE){
    reportProgress('commit', 'git')
    expr <- substitute(
        {
            msg <- message
            systemGit(c('add', '.'))
            systemGit(c('commit', '-a', '-m', shQuote(msg)))  
        },
        list(message = message)
    )
    if(eval) eval(expr) else expr
}

# merge a specific source branch into the current recipient branch
# NB: upstream code must ensure the we have already checked out the recipient branch
mergeFromBranch <- function(sourceBranch, fastForwardOnly = FALSE, eval = FALSE){
    reportProgress('merge', 'git')
    expr <- substitute(
        {
            if(fastForwardOnly){ # fails if branches are diverged
                systemGit(c('merge', '--ff-only', sourceBranch))
            } else {
                systemGit(c('merge', sourceBranch))
            }
        },
        list(sourceBranch = sourceBranch, fastForwardOnly = fastForwardOnly)
    )
    if(eval) eval(expr) else expr
}

