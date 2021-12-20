#----------------------------------------------------------------------
# authorization and other functions for configuration editing
#----------------------------------------------------------------------

checkConfigEditPermission <- function(){
    if(serverEnv$IS_LOCAL) return(TRUE) # can always edit on your own machine
    # TODO: continue to define who gets to edit config files
    if(serverEnv$IS_SERVER) return(FALSE)
    FALSE
}
