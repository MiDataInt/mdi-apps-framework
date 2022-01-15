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
    if(detached) head$sha 
    else paste("branch:", head$name)

    # git <- listGitBranches(localOnly = TRUE, eval = TRUE)
    # if(!git$success) '**ERROR**'
    # else {
    #     x <- git$results[which(startsWith(git$results, '*'))]
    #     strsplit(x, ' ')[[1]][2]
    # }   
}

