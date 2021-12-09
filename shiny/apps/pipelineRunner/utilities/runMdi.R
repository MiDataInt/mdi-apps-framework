#----------------------------------------------------------------------
# run the mdi pipelines command tool from within the apps framework
#----------------------------------------------------------------------

# the mdi utility associated with the same MDI installation as this server
mdiCommandTarget <- file.path(serverEnv$MDI_DIR, 'mdi')

# use system2 to call mdiCommandTarget
runMdiCommand <- function(args = character(), collapse = TRUE){
    if(serverEnv$IS_DEVELOPER) args <- c('-d', args)
    Sys.setenv(IS_PIPELINE_RUNNER = TRUE)
    tryCatch({
        x <- suppressWarnings(system2(
            mdiCommandTarget, 
            args,
            stdout = TRUE,
            stderr = TRUE
        ))
        list(
            success = TRUE, # system2 succeeded, although mdi <command> may have failed
            results = if(collapse) paste(x, collapse = "\n") else x
        )
    }, error = function(e){ # this capture system2 errors, NOT mdi errors (which return in stderr)
        list(
            success = FALSE,
            results = e
        )
    })
}

# determine whether mdi (not system2) reported a usage/configuration error
isMdiSuccess <- function(results){
    !grepl('mdi error:', results) && 
    !grepl('!!!!!!!!!!', results) &&
    !grepl('WARNING!', results)
}
