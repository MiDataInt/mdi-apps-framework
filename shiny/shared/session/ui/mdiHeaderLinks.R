#----------------------------------------------------------------------
# standardized docs, terminal, console, code and settings links 
# for page and box headers
#----------------------------------------------------------------------

mdiHeaderLinks <- function(
    id = NULL,  # id of the app step module if any of the link items are to be shown
    documentation = FALSE, # include a documentation link for this app step
    terminal = FALSE, # include a link to open a context-specific terminal emulator
    console = FALSE,   # include a link to open a context-specific R console
    code = FALSE,      # include a link to open a context-specific code viewer/editor
    settings = FALSE  # include a link to open a settings panel
){
    if(is.null(id)) return("")
    ns <- NS(id)
    HTML(paste( 
        if(documentation) documentationLinkUI(ns('documentation')) else "",
        if(!serverEnv$IS_SERVER && terminal) commandTerminalLink(ns('terminal')) else "",
        if(!serverEnv$IS_SERVER && console) rConsoleLink(ns('console')) else "",
        if(code) aceEditorLink(ns('code')) else "",
        if(settings) stepSettingsUI(ns('settings')) else ""
    ))
}

activateMdiHeaderLinks <- function(
    id,  # id of the app step module if any of the link items are to be shown
    session,
    url = NULL, # the documentation url
    dir = NULL, # include a link to open a context-specific terminal emulator
    envir = NULL,  # include a link to open a context-specific R console
    baseDirs = NULL,     # include a link to open a context-specific code viewer/editor
    settings = FALSE, # include a link to open a settings panel,
    ...              # additional arguments passed to settingsServer
){
    if(!is.null(url)) documentationLinkServer('documentation', url = url)
    if(!serverEnv$IS_SERVER && !is.null(dir)) observeEvent(session$input$terminal, 
        showCommandTerminal(
            session, 
            dir = dir,
            forceDir = TRUE
        )
    )
    if(!serverEnv$IS_SERVER && !is.null(envir)) observeEvent(session$input$console,
        showRConsole(session, envir, id)  
    )
    if(!is.null(baseDirs)) observeEvent(session$input$code, 
        showAceEditor(
            session, 
            baseDirs = baseDirs,
            editable = serverEnv$IS_DEVELOPER
        )
    )
    settings <- if(settings) settingsServer('settings', id, ...)
    settings # the return value
}

getAppStepDir <- function(module, framework = FALSE){
    if(framework) file.path(serverEnv$SHARED_DIR, "session/modules/appSteps", module)
    else file.path(app$DIRECTORY, "modules/appSteps", module)
}

getDocumentationUrl <- function(path, domain = NULL, framework = FALSE){
    file.path(
        "https:/",
        if(framework) "midataint.github.io/mdi-apps-framework" else domain,
        path
    )
}
