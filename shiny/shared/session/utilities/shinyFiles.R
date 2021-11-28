#----------------------------------------------------------------------
# wrapper functions for shinyFiles access to server files
# enforces authorization
#----------------------------------------------------------------------
# note: this is not a module due to the implementation of the shinyFiles package
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
serverFilesButtonServer <- function(id, input, session, filetypes = NULL){
    observe({
        message()
        message("input[[id]]")
        print(input[[id]])
    })
    shinyFileChoose(
        input,
        id,
        updateFreq = 0,
        session = session,
        defaultRoot = NULL,
        defaultPath = "",
        roots = getAuthorizedServerPaths,
        filetypes = filetypes
    )
}

# get the server files paths authorized to this user
getAuthorizedServerPaths <- function(){
    c(TEST = "/srv/mdi")
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
