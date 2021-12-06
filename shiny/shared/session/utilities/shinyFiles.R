#----------------------------------------------------------------------
# shinyFiles wrapper functions to load and save server files, subject to authorization
#----------------------------------------------------------------------
# note: these are _not_ modules due to the implementation of the shinyFiles package
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# a button to find a source file to load, used by sourceFileUpload
#----------------------------------------------------------------------
serverSourceFilesButtonUI <- function(id){
    shinyFilesButton(
        id,
        "Load from Server",
        "Select data package, bookmark, or other source file to import",
        multiple = FALSE,
        buttonType = "default",
        class = NULL,
        icon = NULL,
        style = "width: 100%",
        viewtype = "detail"
    )
}
serverSourceFilesButtonServer <- function(id, input, session, 
                                    rw = "read", filetypes = NULL,
                                    loadFn = function(file) NULL){
    paths <- getAuthorizedServerPaths(rw)
    addServerSourceFilesObserver(id, input, loadFn, paths)
    shinyFileChoose(
        input,
        id,
        session = session,
        defaultRoot = getAuthorizedRootVolume('load_default'),
        roots = paths,
        filetypes = filetypes
    )
}
addServerSourceFilesObserver <- function(id, input, loadFn, paths){
    observeEvent(input[[id]], {
        file <- input[[id]]
        req(file)
        reportProgress('serverSourceFilesObserver')
        loadFn( parseFilePaths(paths, file) )
    })
}

#----------------------------------------------------------------------
# enable bookmark saving
#----------------------------------------------------------------------
serverBookmarkButtonUI <- function(id, label, class, filename = NULL){
    if(is.null(filename)) filename <- app$NAME
    shinySaveButton(
        id,
        label,
        "Save bookmark to server",
        filename = filename,
        filetype = "mdi",
        buttonType = "default",
        class = class,
        icon = icon("download"),
        style = "margin: 0;",
        viewtype = "detail"
    )
}
serverBookmarkButtonServer <- function(id, input, session,
                                       saveFn = function(file) NULL){
    paths <- getAuthorizedServerPaths('write')
    addServerBookmarkObserver(id, input, saveFn, paths)
    shinyFileSave(
        input, 
        id, 
        session = session,
        defaultRoot = getAuthorizedRootVolume('bookmark_default'),
        allowDirCreate = TRUE,
        roots = paths,
        filetypes = 'mdi'
    )
}
addServerBookmarkObserver <- function(id, input, saveFn, paths){
    observeEvent(input[[id]], {
        file <- input[[id]]
        req(file)
        file <- parseSavePath(paths, file)
        req(nrow(file) > 0)    
        reportProgress('serverBookmarkObserver')
        saveFn( file$datapath[1] )
    })
}

#----------------------------------------------------------------------
# generic file saving
#----------------------------------------------------------------------
serverSaveFileButtonUI <- function(id, label, filename, filetype, buttonType = "success"){
    shinySaveButton(
        id,
        label,
        label,
        filename = filename,
        filetype = filetype,
        buttonType = buttonType, # adds class "btn-success", etc.
        viewtype = "detail"
    )
}
serverSaveFileLinkUI <- function(id, label, filename, filetype){
    shinySaveLink(
        id,
        label,
        label,
        filename = filename,
        filetype = filetype,
        viewtype = "detail"
    )
}
serverSaveFileButtonServer <- function(id, input, session, filetype,
                                       default_type = NULL,
                                       saveFn = function(file) NULL){
    paths <- getAuthorizedServerPaths('write')
    addServerSaveFileObserver(id, input, saveFn, paths)
    shinyFileSave(
        input, 
        id, 
        session = session,
        defaultRoot = getAuthorizedRootVolume(default_type),
        allowDirCreate = TRUE,
        roots = paths,
        filetypes = filetype
    )
}
addServerSaveFileObserver <- function(id, input, saveFn, paths){
    observeEvent(input[[id]], {
        file <- input[[id]]
        req(file)
        file <- parseSavePath(paths, file)
        req(nrow(file) > 0)    
        reportProgress('serverSaveFileObserver')
        saveFn( file$datapath[1] ) # just the file path string
    })
}

#----------------------------------------------------------------------
# from the shinyFile documentation for dirGetter and fileGetter, used by shinyFileChoose, etc.
#----------------------------------------------------------------------
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
