
#----------------------------------------------------------------------
# static components for a button to link user to Globus for file selection
#----------------------------------------------------------------------

# module ui function
getGlobusFileButtonUI <- function(id, label) {

    # initialize namespace
    ns <- NS(id)
    
    # only show controls if we have a registered Globus client application and local endpoint
    if(is.null(globusConfig$client$key) ||
       is.null(globusConfig$client$secret)){
        return("Globus file transfers require a client application key (see Globus Setup)")
    }
    if(is.null(globusConfig$endpoint$id)){
        return("Globus file transfers require a local Globus Connect endpoint (see Globus Setup)")
    }
    
    # action button
    bsButton(ns('button'), label, style = "default")
}

