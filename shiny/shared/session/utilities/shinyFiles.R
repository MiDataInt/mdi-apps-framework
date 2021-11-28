#----------------------------------------------------------------------
# wrapper functions for shinyFiles access to server files
# enforces path authorization
#----------------------------------------------------------------------
# note: this is _not_ a module due to the implementation of the shinyFiles package
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# a button to find a file to load
#----------------------------------------------------------------------
serverFilesButtonUI <- function(id){
    shinyFilesButton(
        id,
        "Browse on Server",
        "Select data package, bookmark, or other source file to import",
        multiple = FALSE,
        buttonType = "default",
        class = NULL,
        icon = NULL,
        style = NULL,
        viewtype = "detail"
    )
}
serverFilesButtonServer <- function(id, input, session, 
                                    rw = "read", filetypes = NULL,
                                    loadFn = function(file) NULL){
    addServerFilesObserver(id, input, loadFn)
    shinyFileChoose(
        input,
        id,
        updateFreq = 0,
        session = session,
        defaultRoot = NULL,
        defaultPath = "",
        roots = getAuthorizedServerPaths(rw),
        filetypes = filetypes
    )
}
addServerFilesObserver <- function(id, input, loadFn){
    observeEvent(input[[id]], {
        file <- input[[id]]
        req(file)    
        reportProgress('input[[id]]')
        loadFn(file)
    })
}

#----------------------------------------------------------------------
# get the server file paths authorized to the current, authenticated user
#----------------------------------------------------------------------
getAuthorizedServerPaths <- function(rw = "read"){
    auth <- authenticatedUserData$authorization
    if(is.null(auth) || is.null(auth$paths) || is.null(auth$paths[[rw]])) return( character() )
    paths <- auth$paths[[rw]]
    if(paths == "all") paths <- names(serverConfig$paths)
    unlist(serverConfig$paths[paths])
}

# roots         A named vector of absolute filepaths or a function returning a named vector of
#               absolute filepaths (the latter is useful if the volumes should adapt to changes in
#               the filesystem).
# restrictions  A vector of directories within the root that should be filtered out of the results
# filetypes     A character vector of file extensions (without dot in front i.e. ’txt’ not ’.txt’) to
#               include in the output. Use the empty string to include files with no extension. If
#               not set all file types will be included
# pattern       A regular expression used to select files to show. See base::grepl() for additional 
#               discussion on how to construct a regular expression (e.g., "log.*\\.txt")
#               hidden A logical value specifying whether hidden files should be returned or not
