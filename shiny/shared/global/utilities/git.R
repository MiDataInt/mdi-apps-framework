#----------------------------------------------------------------------
# manipulate an MDI git repository by calls to git2r
# primarily to check out and report on repository versions in running app server
#----------------------------------------------------------------------

# get the name of the current branch, tag or commit, i.e., the location of HEAD
# returns a simple character value
getGitHead <- function(dir){
    if(is.null(dir) || !file.exists(dir)) return(NULL)
    head <- git2r::repository_head(dir)
    detached <- git2r::is_detached(dir)
    if(detached) head$sha # when head is detached, git2r gives the commit id as sha (not the tag name)
    else head$name # when head is branch, git2r just gives name, not a commit or associated tags
}

# checkout (i.e. change to) a specific branch
# do not check or set repo locks here, caller must manage locks as needed
checkoutGitBranch <- function(dir, branch = 'main', create = FALSE, silent = FALSE) {
    if(!silent) message(paste('setting', dir, 'head to', branch))
    git2r::checkout(
        object = dir,
        branch = branch, # git2r calls it 'branch', but can be anything that can be checked out
        force = FALSE,
        create = create
    )
    x <- list() # record the current branch of all repos in environment for rapid checking later
    x[[dir]] <- branch       
    do.call(Sys.setenv, x)
}

# set and clear MDI locks on git repositories
# use same format as mdi-pipelines-framework so locks are shared between Stages 1 and 2
# locks are _not_ fork-specific, i.e., a lock applies equally to definitive and developer-forks
getMdiLockFile <- function(repoDir){
    parts <- rev(strsplit(repoDir, '/')[[1]])
    repo <- parts[1]
    fork <- parts[2] # definitive or developer-forks
    type <- parts[3] # suites or frameworks
    lockFile <- paste(repo, 'lock', sep = ".")
    mdiDir <- paste(rev(parts[4:length(parts)]), collapse = "/")
    file.path(mdiDir, type, lockFile)
}
waitForRepoLock <- function(lockFile = NULL, repoDir = NULL){
    if(is.null(lockFile)) lockFile <- getMdiLockFile(repoDir)
    if(!file.exists(lockFile)) return()  
    message(paste("waiting for lock to clear:", lockFile))  
    maxLockWaitSec <- 30
    cumLockWaitSec <- 0
    while(file.exists(lockFile) && cumLockWaitSec <= maxLockWaitSec){ # wait for others to release their lock
        cumLockWaitSec <- cumLockWaitSec + 1
        Sys.sleep(1);
    }
    if(file.exists(lockFile)){
        message(paste0(
            "\nrepository is locked:\n    ", 
                repoDir,
            "\nif you know the repository is not in use, try deleting its lock file:\n    ", 
                lockFile, "\n"
        ))
        stop('no')
    }
}
setMdiGitLock <- Vectorize(function(repoDir){ # expect that caller has used waitForRepoLock as needed
    lockFile <- getMdiLockFile(repoDir)
    waitForRepoLock(lockFile)
    file.create(lockFile)
})
releaseMdiGitLock <- Vectorize(function(repoDir){
    lockFile <- getMdiLockFile(repoDir)
    if(file.exists(lockFile)) unlink(lockFile)
})

# get the latest/all semantic version tags, i.e., release, of upstream, definitive repos
semVerToSortableInteger <- Vectorize(function(semVer){ # expects vMajor.Minor.Patch
    x <- gsub('v', '', semVer) # Major.Minor.Patch (no 'v')
    x <- as.integer(strsplit(x, "\\.")[[1]])
    x[1] * 1e10 + x[2] * 1e5 + x[3] # thus, most recent versions have the highest integer value
})
# getLatestVersion <- function(tags){ # return most recent version tag as vMajor.Minor.Patch
#     if(length(tags) == 0) return(NA)
#     isSemVer <- grepl('^v{0,1}\\d+\\.\\d+\\.\\d+$', tags, perl = TRUE)
#     semVer <- tags[isSemVer]
#     if(length(semVer) == 0) return(NA)
#     semVerI <- semVerToSortableInteger(semVer)
#     semVer[ which.max(semVerI) ]
# }
getAllVersions <- function(dir) {

# git fetch upstream --tags
# git push origin --tags
# if(dir is forked) fetch(dir, "upstream")
    if(isDeveloperFork(dir))  dstr(git2r::tags(getMatchingDefinitiveRepo(dir)))


    tags <- git2r::tags(dir) # tag (name) = commit data list (value)
    if(length(tags) == 0) return(character())
    tags <- names(tags)
    isSemVer <- grepl('^v{0,1}\\d+\\.\\d+\\.\\d+$', tags, perl = TRUE)
    semVer <- tags[isSemVer]
    if(length(semVer) == 0) return(character())
    semVerI <- semVerToSortableInteger(semVer)
    rev( semVer[ rank(semVerI) ] ) # thus, latest release tag is always first in list
}

# utilities to parse and examine git directories/repos
isDeveloperFork <- function(dir){
    grepl('/developer-forks/', dir)
}
getMatchingDefinitiveRepo <- function(dir){
    gsub('/developer-forks/', '/definitive/', dir)
}
