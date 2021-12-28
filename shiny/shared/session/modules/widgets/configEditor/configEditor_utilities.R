#----------------------------------------------------------------------
# authorization and other functions for configuration editing
#----------------------------------------------------------------------

checkConfigEditPermission <- function(){

    # public servers must be configured externally to any running apps-server instance, by an admin, since:
    #   - cannot push config changes from docker volume to AWS file path
    #   - would need to restart all apps-server instances, not just the one making the config change
    #   - don't wish to expose server configuration on a public url, even if authenticated
    if(serverEnv$IS_SERVER) FALSE

    # can always edit config files on your own machine
    else if(serverEnv$IS_LOCAL) TRUE

    # secure remote server connections allow editing your own, but not hosted, configs
    # this encompasses modes 'remote', 'node' and 'ondemand'

    # TODO: need to manage and ensure HOST_DIR presence and use
    # probably need to APPEND HOST_DIR config to user mdi/config?
    else FALSE

}
