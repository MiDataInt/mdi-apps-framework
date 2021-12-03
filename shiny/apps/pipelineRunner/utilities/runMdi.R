#----------------------------------------------------------------------
# run the mdi pipelines command tool from within the apps framework
#----------------------------------------------------------------------
mdiCommandTarget <- file.path(serverEnv$MDI_DIR, 'mdi')
runMdiCommand <- function(args = character()){
    if(serverEnv$IS_DEVELOPER) args <- c('-d', args)
    tryCatch({
        x <- suppressWarnings(system2(
            mdiCommandTarget, 
            args,
            stdout = TRUE
        ))
        list(
            success = TRUE,
            results = paste(x, collapse = "\n")
        )
    }, error = function(e){
        list(
            success = FALSE,
            results = e
        )
    })
}
