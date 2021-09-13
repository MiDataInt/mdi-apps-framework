#----------------------------------------------------------------------
# target app loading; activated by file inputs on launch page
#----------------------------------------------------------------------
loadRequest <- reactiveVal(list())
observeLoadRequest <- observeEvent(loadRequest(), {
    req(loadRequest()$app)
    startSpinner(session, 'observeLoadRequest') 
    NAME <- loadRequest()$app
    DIRECTORY <- paste('..', 'apps', appFamilies[NAME], NAME, sep="/")

    # in developer mode only, switch to a (new) branch dedicated to this app
    # never change branches in production, stay on main always
    if(serverEnv$IS_DEVELOPER) switchGitBranch(NAME, session, sessionEnv, CONSTANTS)
    isolate(invalidateGitBranch( invalidateGitBranch() + 1 ))        
    
    # initialize the configuration of the requested app
    app$NAME <<- NAME
    app$DIRECTORY <<- DIRECTORY
    app$info <<- read_yaml(file.path(DIRECTORY, 'config.yml'))
    loadAppScriptDirectory(DIRECTORY) # add all scripts defined within the app itself

    # validate and establish the module dependency chain
    failure <- initializeAppStepNamesByType()
    if(!is.null(failure)){
        message()
        message(rep('!', 80))
        message(paste(NAME, 'app config error:', failure))
        message(rep('!', 80))
        message()
        return( stopSpinner(session) )
    }
    initializeDescendants()

    # load all the optional modules and classes required by the app's modules
    required <- list(modules=list(), classes=list())
    loadTypes <- names(required)
    for(appStep in app$info$appSteps){ # ensure that all appStep modules are loaded even if not listed in config.yml
        moduleInfo <- stepModuleInfo[[appStep$module]]
        for(loadType in loadTypes){
            if(is.null(moduleInfo[[loadType]])) next
            for(type in names(moduleInfo[[loadType]])) {
                required[[loadType]][[type]] <- c( required[[loadType]][[type]], moduleInfo[[loadType]][[type]] )
            }            
        }
        required$modules$appSteps <- c( required$modules$appSteps, appStep$module ) # load optional appSteps modules also
    }  
    for(loadType in loadTypes){
        for(type in names(required[[loadType]])){ # modules, organized by moduleType
            for(target in unique(required[[loadType]][[type]])){
                if(type == 'appSteps' && exists(paste0(target, 'UI'))) next # app can override standard modules (module of this name already loaded)
                loadAllRScripts(file.path('optional', loadType, type, target), recursive=TRUE)
            }        
        }     
    }

    # enable developer interface in local mode only
    if(serverEnv$IS_LOCAL && serverEnv$IS_DEVELOPER) {
        dir <- file.path('optional', 'modules', 'developerTools')
        loadAllRScripts(dir, recursive=TRUE)
        addDeveloperMenuItem()
    }
    
    # determine the best way to initialize the UI for this user and incoming file
    nAppSteps <- length(app$info$appSteps)
    appStepNames <- names(app$info$appSteps)
    userFirstVisit <- is.null(cookie) || is.null(cookie[[app$NAME]]) || cookie[[app$NAME]] != 'true'
    isBookmarkFile <- loadRequest()$file$type == "bookmark"
    showSplashScreen <- !isBookmarkFile && (nAppSteps == 0 || (userFirstVisit && !serverEnv$IS_DEVELOPER))      
    splashScreenName <- 'appName' # the name of the app overview tab
    selectedStep <- if(showSplashScreen) splashScreenName else {
        path <- loadRequest()$file$path
        stepName <- if(isBookmarkFile) getTargetAppFromBookmarkFile(path, function(...) NULL)$step else NULL
        if(is.null(stepName)) appStepNames[1] else stepName
    }
    nAboveFold <- if(selectedStep == splashScreenName) 1 else which(appStepNames == selectedStep)  
    if(length(nAboveFold) == 0) { # failsafe in case bookmark provides a bad step name
        selectedStep <- splashScreenName
        nAboveFold <- 1 # even if showing splash screen, must immediately load app step 1 so it can handle the incoming source file
    }

    # initialize app-specific data paths
    initializeAppDataPaths()      
    
    # initialize the app-specific sidebar menu
    removeUI(".sidebar-menu li, #saveMagcFile-saveMagcFile, .sidebar-status",
             multiple=TRUE, immediate=TRUE)
    insertUI(".sidebar-menu", where = "beforeEnd", immediate = TRUE,
        ui = tagList(
            menuItem(tags$div(app$info$name, class="app-name"), tabName="appName"), # app name, links to Overview
            if(nAppSteps > 0) lapply(1:nAppSteps, sequentialMenuItem), # app-specific steps
            bookmarkingUI('saveMagcFile', list(class="sidebarBookmarking")), # enable bookmarking, i.e. saving app state
            if(serverEnv$IS_DEVELOPER) sibebarStatusUI('frameworkStatus') else ""
        )
    )      

    # initialize the app-specific content panels
    removeUI(".tab-content", immediate = TRUE)
    insertUI(".content",  where = "beforeEnd", immediate = TRUE,   
        ui = eval({ 
            tabItemsList <- getAppOverviewHtml(nAppSteps) # brief general description of app
            if(nAppSteps > 0) for(i in 1:nAppSteps){ # one tab item per app-specific analysis step
                tabItemsList[[length(tabItemsList) + 1]] <- sequentialTabItem(i)
            }  
            do.call(tabItems, unname(tabItemsList)) # do.call syntax necessitated by a limitation of shinydashboard          
        })
    )
    
    # initialize the record lock lists
    locks <<- intializeStepLocks()
    
    # enable bookmarking; appStep modules react to bookmark
    bookmark <<- bookmarkingServer('saveMagcFile', locks) # in the app sidebar

    # load servers for all required appStep modules, plus finally run appServer
    # because this is the slowest initialization step, defer many until after first UI load
    if(!exists('appServer')) appServer <- function() NULL # for apps with no specific server code
    runModuleServers <- function(startI, endI){
        lapply(startI:endI, function(i){
            stepName <- names(app$info$appSteps)[i]
            reportProgress(paste('loadStepModuleServers', stepName))            
            step <- app$info$appSteps[[i]]
            server <- get(paste0(step$module, 'Server'))
            step$options$stepNumber <- i
            app[[stepName]] <<- server(stepName, step$options, bookmark, locks)
            addStepReadinessObserver(stepName)
        })
    }
    if(nAppSteps > 0){
        if(nAboveFold > 0) runModuleServers(1, nAboveFold)
        if(nAppSteps > nAboveFold) {
            reportProgress('-- THE FOLD --')
            future({ Sys.sleep(2) }) %...>% (function(x) {
                runModuleServers(nAboveFold + 1, nAppSteps)
                appServer()
            })   
        } else appServer()       
    } else appServer() 

    # select the first content page
    updateTabItems(session, 'sidebarMenu', selected = selectedStep) 
    
    # enable a universal action to close any modal dialog/popup
    addRemoveModalObserver(input)        
    
    # enable additional feedback in the sidebar
    if(serverEnv$IS_DEVELOPER) sibebarStatusServer('frameworkStatus')    
    
    # push the initial file upload to the app via it's first step module
    if(loadRequest()$file$type == CONSTANTS$sourceFileTypes$bookmark){
        bookmark$file <- loadRequest()$file$path
        nocache <- loadRequest()$file$nocache
        if(is.null(nocache) || !nocache) bookmarkHistory$set(file=bookmark$file) # ensure loaded bookmark files appear in the cache list
    } else {
        firstStep <- app[[ names(app$info$appSteps)[1] ]]        
        firstStep$loadSourceFile(loadRequest()$file)
    } 
    
    # clean up
    stopSpinner(session, 'observeLoadRequest')
    observeLoadRequest$destroy() # user has to reload page to reset to launch page once an app is loaded
    session$sendCustomMessage('setDocumentCookie', list(
        name  = app$NAME,
        data  = list(value = TRUE, isServerMode = serverEnv$IS_SERVER),
        nDays = 365
    ))
})

