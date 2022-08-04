#----------------------------------------------------------------------
# if present, must contain a single function called 'appServer',
# which is called in session context and thus has implicit access to:
#   input, output, session
#   return values from all app step modules
#----------------------------------------------------------------------
# if not needed, simply omit file server.R from your app
#----------------------------------------------------------------------

# objects instantiated here are available to all appStep modules in a session

# wrapper around documentationLinkServer to support docs links
addPRDocs <- function(id, docPath, anchor = NULL){
    documentationLinkServer(id, "wilsonte-umich", "mdi-apps-framework", docPath, anchor)
}

# appServer function called after all modules are instantiated
appServer <- function(){


}
