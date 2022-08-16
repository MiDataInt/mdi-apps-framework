#----------------------------------------------------------------------
# manipulate an MDI git repository by calls to git2r
# to check out and report on repository versions in the running app server
#----------------------------------------------------------------------

# get the current branch, tag, or commit, i.e., the location of HEAD
getGitHead <- function(repo){ # repo = gitStatusData$suite|framework, with dir and versions filled
    if(is.null(repo$dir) || !file.exists(repo$dir)) return(NULL)
    head <- git2r::repository_head(repo$dir)
    if(git2r::is_detached(repo$dir)){
        if(is.null(repo$versions)) return(NULL)
        if(head$sha %in% repo$versions){
            list(
                type = 'version',
                version = names(repo$versions)[repo$versions == head$sha],
                sha = head$sha # always return the commit id, i.e., sha
            )
        } else {
            list(
                type = 'commit',
                commit = getShortGitCommit(head$sha), # abbreviated for display purposes
                sha = head$sha
            )
        }
    } else {
        list(
            type = 'branch',
            branch = head$name,
            sha = git2r::branch_target(head) 
        )        
    }
}
getShortGitCommit <- function(sha) substr(sha, 1, 8)
getGitHeadDisplay <- function(head) {
    if(is.null(head)) return("")
    head[[head$type]]
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
        Sys.sleep(1)
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
getAllVersions <- function(dir) {
    if(isDeveloperFork(dir)) dir <- getMatchingDefinitiveRepo(dir)
    tags <- git2r::tags(dir) # tag (name) = commit data list (value)
    if(length(tags) == 0) return(character())
    isSemVer <- grepl('^v{0,1}\\d+\\.\\d+\\.\\d+$', names(tags), perl = TRUE)
    semVer <- tags[isSemVer]
    if(length(semVer) == 0) return(character())
    semVerI <- rank(semVerToSortableInteger(names(semVer)))
    # thus, latest release tag is always first in list
    # name = version, value = commit id/sha
    sapply(rev( semVer[semVerI] ), function(x) x$sha)
}

# utilities to parse and examine git directories/repos
isDeveloperFork <- function(dir) grepl('/developer-forks/', dir)
getMatchingDefinitiveRepo <- function(dir) gsub('/developer-forks/', '/definitive/', dir)
