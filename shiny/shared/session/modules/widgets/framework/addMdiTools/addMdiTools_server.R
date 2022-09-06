#----------------------------------------------------------------------
# reactive components for constructing a modal panel to add or create MDI tool suites and apps
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
addMdiToolsServer <- function(
    id
){
    moduleServer(id, function(input, output, session){
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize the module
#----------------------------------------------------------------------
module <- "addMdiTools"
observers <- list() # for module self-destruction
githubPrefix <- "https://github.com/"
suitesConfigFile <- file.path(serverEnv$MDI_DIR, "config", "suites.yml")
suitesYamlPrefix <- "---
#----------------------------------------------------------------------
# Tool suites to install
#----------------------------------------------------------------------
#   - entries should point to GitHub repositories
#   - developers should _not_ list their repo forks here
#   - when a pipeline/app name is in multiple suites, the first match is used
#----------------------------------------------------------------------"
restartServer <- function(message){
    stopSpinner(session)
    showUserDialog(
        "Server Restart Required", 
        tags$p(
            "Please reload a fresh web page to start a new session including ", 
            "the newly ",
            message,
            " once the server restarts."
        ),
        callback = function(...) {
            Sys.setenv(MDI_FORCE_RESTART = "TRUE")
            Sys.setenv(MDI_FORCE_REINSTALLATION = "TRUE")
            removeModal()
            stopApp()
        },
        size = "s", 
        type = 'okOnlyCallback', 
        footer = NULL, 
        easyClose = FALSE,
        removeModal = FALSE
    ) 
}

#----------------------------------------------------------------------
# initialize the suite selectInput
#----------------------------------------------------------------------
if(serverEnv$IS_DEVELOPER) selfDestruct <- observe({

    # collect all known suite directories with fork/suite display names
    dirs <- unique(c(unlist(getPipelineSuiteDirs()), unlist(getAppSuiteDirs())))
    dirs <- dirs[!startsWith(dirs, serverEnv$FRAMEWORKS_DIR)]
    suitesDir <- paste0(serverEnv$SUITES_DIR, '/')
    choices <- list()
    sapply(dirs, function(dir) choices[[gsub(suitesDir, "", dir)]] <<- dir)

    # remove definitive forks if developer forks exist
    choices <- rev(choices[order(names(choices))])
    seen <- list()
    for(choice in names(choices)){
        suiteName <- rev(strsplit(choice, "/")[[1]])[1]
        if(!is.null(seen[[suiteName]])) choices[[choice]] <- NULL
        seen[[suiteName]] <- TRUE
    }

    # fill the suite selector
    updateSelectInput(session, "toolSuite", choices = rev(choices))
    selfDestruct$destroy()
})

#----------------------------------------------------------------------
# all users: add a tool suite
#----------------------------------------------------------------------
observers$addToolSuite <- observeEvent(input$addToolSuite, {

    # validate the requested suite URL
    url <- trimws(input$githubUrl)
    req(url)
    parts <- strsplit(url, '/')[[1]]
    if(!startsWith(url, githubPrefix) ||
       length(parts) != 5) return(html(
        "addSuiteError",
        "not a valid GitHub repository URL"
    ))  

    # make sure suite isn't already installed
    suites <- read_yaml(suitesConfigFile) 
    suiteUrls <- sapply(suites$suites, function(x){
        if(startsWith(x, githubPrefix)) x else paste0(githubPrefix, x)
    })
    if(url %in% suiteUrls) return(html(
        "addSuiteError",
        "the suite is already installed on this server"
    )) 

    # commit the updated suites.yml
    html("addSuiteError", "")
    startSpinner(session, message = "updating server config")
    suites$suites <- c(suites$suites, url)
    yaml <- as.yaml(suites) 
    cat(suitesYamlPrefix, yaml, file = suitesConfigFile, sep = "\n")

    # restart the server to install the new suite
    restartServer("installed tool suite")
}, ignoreInit = TRUE)

#----------------------------------------------------------------------
# developers: add a tool to a suite
#----------------------------------------------------------------------
addTool <- function(type, toolsPath, configFile){

    # check if the tool already exists
    toolName <- trimws(input$toolName)
    req(toolName)
    if(grepl('\\s', toolName)) return(html(
        "addToolError",
        "tool names may not contain spaces"
    ))  
    toolsDir <- file.path(input$toolSuite, toolsPath)
    existingTools <- list.files(toolsDir)
    if(toolName %in% existingTools) return(html(
        "addToolError",
        paste(if(type == "app") "an" else "a", type, "named '", toolName, 
              "' already exists in the selected suite")
    ))  

    # copy the mininimal template to the new tool
    html("addToolError", "")
    removeModal()
    startSpinner(session, message = paste("creating new", type))
    templateDir <- file.path(serverEnv$SHARED_DIR, 'templates', type)
    toolDir <- file.path(toolsDir, toolName)
    R.utils::copyDirectory(from = templateDir, to = toolDir)

    # update the tool name and description into the config file(s)
    toolDescription <- trimws(input$toolDescription)
    if(is.null(toolDescription) || toolDescription == "") toolDescription <- paste(toolName, type)
    configFile <- file.path(toolDir, configFile)
    config <- loadResourceText(configFile)
    config <- gsub('__TOOL_NAME__', toolName, config)
    config <- gsub('__TOOL_DESCRIPTION__', toolDescription, config)
    cat(gsub('\\r', '', config), file = configFile)

    # restart the server to learn about the new tool
    restartServer(paste("created", type))
}
observers$addPipeline <- observeEvent(input$addPipeline, {
    addTool('pipeline', 'pipelines', 'pipeline.yml')
})
observers$addApp <- observeEvent(input$addApp, {
    addTool('app', 'shiny/apps', 'config.yml')
})

#----------------------------------------------------------------------
# developers: add components to tools
#----------------------------------------------------------------------
nameMessage   <- "please enter a component name"
existsMessage <- "component exists or was just created successfully"
invalidateComponentDir <- reactiveVal(0)
componentDir <- reactive({
    invalidateComponentDir()
    name <- trimws(input$componentName)
    if(name == "") return(nameMessage)
    dir <- if(input$sharedComponent) file.path(gitStatusData$suite$dir, 'shiny/shared/session/modules') 
           else file.path(app$DIRECTORY, "modules")
    dir <- switch(
        input$componentType,
        appStep = file.path(dir, "appSteps", name),
        widget  = file.path(dir, "widgets",  name)
    ) 
    if(dir.exists(dir) || file.exists(dir)) existsMessage else dir
})
output$componentDir <- renderText({
   componentDir()
})
observers$addComponent <- observeEvent(input$addComponent, {
    dir <- componentDir()
    req(!(dir %in% c(nameMessage, existsMessage)))
    dir.create(dir, recursive = TRUE)
    name <- basename(dir)
    tmpDir <- file.path(serverEnv$SHARED_DIR, 'templates', input$componentType)
    createModuleFile <- function(type, ext){
        fileName <- if(ext == "R") paste0("module_", type, ".", ext)
                              else paste0(type, ".", ext)
        file <- file.path(tmpDir, fileName)
        if(!file.exists(file)) return()
        txt <- slurpFile(file)
        txt <- gsub("\\r", "", txt)
        txt <- gsub("__MODULE_NAME__", name, txt)
        txt <- gsub("__IS_SHARED_MODULE__", if(input$sharedComponent) "TRUE" else "FALSE", txt)
        fileName <- if(ext == "R") paste0(name, "_", type, ".", ext)
                              else fileName
        cat(txt, file = file.path(dir, fileName))
    }
    createModuleFile('ui', 'R')
    createModuleFile('server', 'R')
    createModuleFile('module', 'yml')
    createModuleFile('README', 'md')
    invalidateComponentDir( invalidateComponentDir() + 1 )
})

#----------------------------------------------------------------------
# return value
#----------------------------------------------------------------------
list(
    observers = observers, # for use by destroyModuleObservers
    onDestroy = function() {
        list(  # return the module's cached state object
        )               
    }
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
