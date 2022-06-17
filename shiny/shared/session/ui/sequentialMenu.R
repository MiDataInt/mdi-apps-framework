#----------------------------------------------------------------------
# sequential analysis menu tools
#----------------------------------------------------------------------

# overview of app shown at first user encounter or when the app name is clicked
getAppOverviewHtml <- function(nAppSteps){
    docs <- app$config$documentationUrl
    list(tabItem(tabName = "appName",
        tags$div(class = "text-block",
            tags$h3(HTML(paste(
                app$config$name, 
                "overview",
                if(is.null(docs)) "" else tags$a(
                    href = docs, 
                    style = "font-size: 0.7em; margin-left: 10px;", 
                    target = "mdi_docs",
                    icon('book')
                )
            ))),
            includeMarkdown(file.path(app$DIRECTORY, 'overview.md')),
            tagList(
                tags$h3("Analysis steps"),
                tags$p(paste("The", app$config$name, "app will lead you through the following  sequential steps.")),
                tags$table(class = "overview-table",
                    if(nAppSteps > 0) lapply(app$config$appSteps, function(step){
                        if(step$module == "developerTools") "" else tags$tr(
                            tags$td(getStepOptionValue(step, 'shortLabel')),
                            tags$td(HTML(getStepOptionValue(step, 'shortDescription')))
                        )
                    })                          
                ),
                tags$h3("Save your work!"),
                tags$p(HTML("At any time during your work with the app, click <strong>Save Your Work</strong> in the side panel to save a bookmark file with your current settings. Later, you can upload that file to restart where you left off.")), # nolint
            ),                            
            tags$br(), tags$br(), tags$br()
        )
    ))
}

# the elements in the dashboard menu, one for each analysis step
getStepOptionValue <- function(step, key){
    value <- step[[key]]
    if(is.null(value)) value <- stepModuleInfo[[step$module]][[key]]
    value
}
sequentialMenuItem <- function(stepI){
    step <- app$config$appSteps[[stepI]]
    name <- names(app$config$appSteps)[stepI]
    menuItem(paste(stepI, '-', getStepOptionValue(step, 'shortLabel')), 
             tabName = name, selected = step$selected)
}

# the pages (tabs) shown as each menu item is clicked
sequentialTabItem <- function(stepI){ 
    step <- app$config$appSteps[[stepI]]
    name <- names(app$config$appSteps)[stepI]
    if(is.null(step$options)) step$options <- list()
    step$options$analysisTypes <- app$config$analysisTypes # for steps that need it
    alwaysVisible <- if(is.null(step$options$alwaysVisible) || !step$options$alwaysVisible) 'false' else 'true'
    source <- if(is.null(step$options$source)) "NO_SOURCE" else step$options$source
    tabItem(
        name,
        # if ready, show the app-specific content
        conditionalPanel(
            condition = paste0(alwaysVisible, ' || ', " window.stepIsReady['", source, "'] === true"),            
            get(paste0(step$module, 'UI'))(name, step$options),
            tags$div("", style = "margin-top: 100px;"), # create some padding at the bottom of all app tab pages 
        ),
        # if not, show generic "we're not ready yet" feeback
        conditionalPanel(
            condition = paste0('!', alwaysVisible, ' && ', " window.stepIsReady['", source, "'] !== true"),            
            tags$h3('Pending'),
            tags$div(class = "text-block", paste( 
                getStepOptionValue(step, 'shortLabel'), 
                "will be available when Step #", stepI - 1, "has been completed."
            ))
        ),
        ""
    )
}

# define the typical, common, recommended tab item UI layout ( called from step$ui() )
standardSequentialTabItem <- function(pageTitle, leaderText, ...){
    tagList(
        tags$h3(pageTitle), # step page title
        tags$div(class = "text-block", leaderText), # top level instructions and hints list 
        ... # specific ui content for this module/step
    )   
}

# determine whether a step, as well as the source(s) of that step, are executed and ready
# this function is called in the isReady reactive returned by a step module
# it establishes the dependency chain for step visibility
getStepReadiness <- function(source=NULL, list=NULL, fn=NULL, ...){
    sourceIsReady <- is.null(source) || is.null(app[[source]]$isReady) || app[[source]]$isReady()
    stepListIsReady <- is.null(list) || length(list) > 0
    stepFunctionIsReady <- is.null(fn) || fn(...) # fn must return a logical
    sourceIsReady && stepListIsReady && stepFunctionIsReady

}
stepIsReady <- reactiveValues()
addStepReadinessObserver <- function(stepName){
    isReady <- app[[stepName]]$isReady
    if(is.null(isReady)) {
        stepIsReady[[stepName]] <- TRUE
    } else {
        observeEvent(isReady(), {
            isReady <- isReady()
            stepIsReady[[stepName]] <- isReady # step readiness for use in R code
            session$sendCustomMessage(
                'updateTriggerArray', # step readiness for use in javascript code, especially conditional triggers # nolint
                list(
                    name  = 'stepIsReady',
                    index = stepName,
                    value = isReady 
                ) 
            )
        })        
    }
}

# set the one analysis step that matches a specific module type
# this is the function that matches app config module requests to module scripts
initializeAppStepNamesByType <- function(){
    modulesDirs <- c(
        file.path(app$DIRECTORY, 'modules', 'appSteps'), # app can override standard modules, etc.   
        file.path(app$sources$suiteSharedModulesDir, 'appSteps'),
        file.path(serverEnv$SHARED_DIR, 'session',  'modules', 'appSteps')
    )    
    getStepModuleDir <- function(moduleName){
        for(dir in modulesDirs){ # return the first found module in modulesDirs search order
            moduleDir <- file.path(dir, moduleName)
            if(dir.exists(moduleDir)) return(moduleDir)
        }
        NULL
    }    
    for(i in seq_along(app$config$appSteps)){
        appStep <- app$config$appSteps[[i]]
        stepName <- names(app$config$appSteps)[i] 
        if(is.null(appStep$module)) return(paste("missing module for app step:", stepName))
        moduleDir <- getStepModuleDir(appStep$module)
        if(is.null(moduleDir)) return(paste("unknown module:", appStep$module))
        moduleYml <- file.path(moduleDir, 'module.yml')       
        if(!file.exists(moduleYml)) return(paste("missing module.yml config file for module:", appStep$module))
        stepModuleInfo[[appStep$module]] <<- read_yaml(moduleYml)
        moduleInfo <- stepModuleInfo[[appStep$module]]
        if(is.null(moduleInfo$types)) return(paste("missing type(s) for module:", appStep$module))
        if(is.null(moduleInfo$sourceTypes)) moduleInfo$sourceTypes <- character()
        for(type in moduleInfo$types) appStepNamesByType[[type]] <<- stepName       
        for(sourceType in moduleInfo$sourceTypes){
            if(is.null(appStepNamesByType[[sourceType]])) return(
                paste(appStep$module, 'depends on earlier module of type', sourceType)
            )
        }
        if(length(moduleInfo$sourceTypes) == 1) { # set options$source for appStep modules with one direct parent (mostly for legacy modules) # nolint
            app$config$appSteps[[i]]$options$source <<- appStepNamesByType[[moduleInfo$sourceTypes]]
        }
    }
    return(NULL)
}
getAppStepNameByType <- function(stepType){
    appStepNamesByType[[stepType]]
}
getAppStepByType <- function(stepType){
    stepName <- appStepNamesByType[[stepType]]
    if(is.null(stepName)) return(NULL)
    app[[stepName]]
}
getStepReturnValueByType <- function(stepType, valueName){
    step <- getAppStepByType(stepType)
    req(step)
    value <- step[[valueName]]
    req(value)
    value # returns a reactive (not its value)
}
getStepSettingsByType <- function(stepType) getStepReturnValueByType(stepType, 'settings')
getStepOutcomesByType <- function(stepType) getStepReturnValueByType(stepType, 'outcomes')

# programmatically activate a specific tab/step
activateTab <- function(stepName){
    updateTabItems(session, 'sidebarMenu', stepName)
}

# reactive to report on the currently active tab
activeTab <- reactive({ input$sidebarMenu })

# functions to report on tab statuses
isActiveTab <- function(stepOptions){ # is the query tab/step currently active in UI?
    input$sidebarMenu == names(app$config$appSteps)[stepOptions$stepNumber]
}
isVisibleTab <- function(stepOptions){ # is the query tab/step allowed to be accessed in the UI?
    alwaysVisible <- if(is.null(stepOptions$alwaysVisible) || !stepOptions$alwaysVisible) FALSE else TRUE
    stepIsReady[[stepOptions$source]] || alwaysVisible
}
isRequiredTab <- function(stepOptions){ # is the query tab/step in the series of steps up to and including the currently active one # nolint
    stepOptions$stepNumber <= which(names(app$config$appSteps) == input$sidebarMenu)
}
isActiveVisibleTab <- function(stepOptions) {
    isActiveTab(stepOptions) && isVisibleTab(stepOptions)
}
isRequiredVisibleTab <- function(stepOptions) {
    isRequiredTab(stepOptions) && isVisibleTab(stepOptions)
}
