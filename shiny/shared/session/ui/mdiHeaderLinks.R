#----------------------------------------------------------------------
# standardized dialog and other links wrapped for use in page and box headers
#----------------------------------------------------------------------

# wrapper around header link UI functions
mdiHeaderLinks <- function(
    id = NULL,  # id of the app step module if any of the link items are to be shown
    type = c("page", "box"), # the kind of header into which icons are being place
    documentation = FALSE, # include a documentation link for this app step
    reload = FALSE, # include a link to reload/refresh/sync the module
    code = FALSE,      # include a link to open a context-specific code viewer/editor    
    console = FALSE,   # include a link to open a context-specific R console
    terminal = FALSE, # include a link to open a context-specific terminal emulator
    download = FALSE, # include a link to download the module contents
    settings = FALSE  # include a link to open a settings panel
){
    if(is.null(id)) return("")
    ns <- NS(id)
    class <- paste(type[1], "header-link", sep = "-")
    HTML(paste( 
        if(documentation) documentationLinkUI(ns('documentation'), class = class) else "",
        if(reload) actionLink(ns("reload"), label = icon("sync", verify_fa = FALSE), class = class) else "",
        if(code) aceEditorLink(ns('code'), class = class) else "",        
        if(!serverEnv$IS_SERVER && console) rConsoleLink(ns('console'), class = class) else "",
        if(!serverEnv$IS_SERVER && terminal) commandTerminalLink(ns('terminal'), class = class) else "",
        if(download) downloadLink(ns("download"), label = icon("download"), class = class) else "",
        if(settings) stepSettingsUI(ns('settings'), class = class) else ""
    ))
}

# wrapper around header link server functions
activateMdiHeaderLinks <- function(
    id,  # id of the app step module if any of the link items are to be shown
    session,
    url = NULL, # the documentation url
    reload = NULL, # callback function with no arguments to handle the reload action
    baseDirs = NULL, # include a link to open a context-specific code viewer/editor
    envir = NULL,  # include a link to open a context-specific R console
    dir = NULL, # include a link to open a context-specific terminal emulator
    download = NULL, # download handler for the download link, created with shiny::downloadHandler()
    settings = FALSE, # include a link to open a settings panel,
    ...              # additional arguments passed to settingsServer
){
    if(!is.null(url)) documentationLinkServer('documentation', url = url)
    if(!is.null(reload)) observeEvent(session$input$reload, reload())
    if(!is.null(baseDirs)) observeEvent(session$input$code, 
        showAceEditor(
            session, 
            baseDirs = baseDirs,
            editable = serverEnv$IS_DEVELOPER
        )
    )
    if(!serverEnv$IS_SERVER && !is.null(envir)) observeEvent(session$input$console,
        showRConsole(session, envir, id)  
    )
    if(!serverEnv$IS_SERVER && !is.null(dir)) observeEvent(session$input$terminal, 
        showCommandTerminal(
            session, 
            dir = dir,
            forceDir = TRUE
        )
    )
    if(!is.null(download)) session$output$download <- download
    settings <- if(settings) settingsServer('settings', id, ...)
    settings # the return value
}
