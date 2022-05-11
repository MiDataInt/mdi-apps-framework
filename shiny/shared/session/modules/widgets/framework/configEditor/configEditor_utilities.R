#----------------------------------------------------------------------
# authorization and other functions for configuration editing
#----------------------------------------------------------------------

checkConfigEditPermission <- function(){

    # public servers must be configured by an admin externally to any running apps-server instance, since:
    #   - cannot push config changes from docker volume to AWS file path
    #   - would need to restart all apps-server instances, not just the one making the config change
    #   - don't wish to expose server configuration on a public url, even if authenticated
    if(serverEnv$IS_SERVER) FALSE

    # always allow users to edit the configuration on their own machine
    else if(serverEnv$IS_LOCAL) TRUE

    # allow editing of your own, but not hosted, installations on remote server connections via ssh
    # this encompasses modes 'remote', 'node' and 'ondemand'
    else !serverEnv$IS_HOSTED
}
