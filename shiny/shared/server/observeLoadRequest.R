#----------------------------------------------------------------------
# target app loading; activated by file inputs on launch page
#----------------------------------------------------------------------
loadRequest <- reactiveVal(list())
observeLoadRequest <- observeEvent(loadRequest(), {
    req(loadRequest()$app)
    startSpinner(session, 'observeLoadRequest') 

    # authorize the requested app
    NAME <- loadRequest()$app
    DIRECTORY <- appDirs[[NAME]] # app working directory, could be definitive or developer    
    if(serverEnv$REQUIRES_AUTHENTICATION && !isAuthorizedApp(NAME)) {
        stopSpinner(session, 'observeLoadRequest: unauthorized')
        showUserDialog(
            "Unauthorized App", 
            tags$p(paste("You are not authorized to use the", NAME, "app.")), 
            type = 'okOnly'
        )
        return()
    }

    # initialize the requested app
    app$NAME <<- NAME
    app$DIRECTORY <<- DIRECTORY
    app$sources <<- parseAppDirectory(app$DIRECTORY)
    app$config <<- read_yaml(file.path(DIRECTORY, 'config.yml'))

    # load all relevant session scripts in reverse precedence order
    #   global, then session, folders were previously sourced by initializeSession.R on page load
    loadAllRScripts(app$sources$suiteGlobalDir, recursive = TRUE)
    loadAppScriptDirectory(app$sources$suiteSessionDir)
    loadAppScriptDirectory(DIRECTORY) # add all scripts defined within the app itself; highest precedence

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

    # TODO: continue streamlining git; only switch to legacy tags, no developer switching, etc.
    # remove deprecated in-app developer tools; only a few remain (e.g., keep sandbox, lose git manager)
    # update sidebar status reporting (all apps can report suite/app/version even in run()?)

    # key target scripts still needing work are utilities/git.R and developer/*

    # reinstate developer tools addition (below) later, once apps are running in new framework

    # this includes sidebarStatusUI and Server in lines further below

    # # enable developer interface in private modes only
    # if(!serverEnv$IS_SERVER && serverEnv$IS_DEVELOPER) {
    #     loadAppScriptDirectory('developer')
    #     addDeveloperMenuItem()
    # }

    # determine the best way to initialize the UI for this user and incoming file
    nAppSteps <- length(app$config$appSteps)
    appStepNames <- names(app$config$appSteps)
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
        nAboveFold <- 1 # even if showing splash screen, load app step 1 to handle the incoming source file
    }

    # initialize app-specific data paths
    initializeAppDataPaths()      

    # initialize the app-specific sidebar menu
    removeUI(".sidebar-menu li, #saveBookmarkFile-saveBookmarkFile, .sidebar-status",
             multiple = TRUE, immediate = TRUE)
    insertUI(".sidebar-menu", where = "beforeEnd", immediate = TRUE,
        ui = tagList(
            menuItem(tags$div(app$config$name, class = "app-name"), tabName = "appName"), # app name, links to Overview
            if(nAppSteps > 0) lapply(1:nAppSteps, sequentialMenuItem), # app-specific steps
            saveYourWorkLinks()  # enable state bookmarking
            # if(serverEnv$IS_DEVELOPER) sibebarStatusUI('frameworkStatus') else ""
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
            do.call(tabItems, unname(tabItemsList)) # do.call syntax necessitated shinydashboard limitation  
        })
    )
    
    # initialize the record lock lists
    locks <<- intializeStepLocks()
    
    # enable bookmarking; appStep modules react to bookmark
    bookmark <<- bookmarkingServer('saveBookmarkFile', list(), locks) # in the app sidebar
    if(serverEnv$IS_SERVER) serverBookmark <<- bookmarkingServer('saveBookmarkToServer', list(shinyFiles = TRUE), locks)

    # load servers for all required appStep modules, plus finally run appServer
    # because this is the slowest initialization step, defer many until after first UI load
    if(!exists('appServer')) appServer <- function() NULL # for apps with no specific server code
    runModuleServers <- function(startI, endI){
        lapply(startI:endI, function(i){
            stepName <- names(app$config$appSteps)[i]
            reportProgress(paste('loadStepModuleServers', stepName))            
            step <- app$config$appSteps[[i]]
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
    #if(serverEnv$IS_DEVELOPER) sibebarStatusServer('frameworkStatus')    
    
    # push the initial file upload to the app via it's first step module
    if(loadRequest()$file$type == CONSTANTS$sourceFileTypes$bookmark){
        bookmark$file <- loadRequest()$file$path
        nocache <- loadRequest()$file$nocache
        if(is.null(nocache) || !nocache) bookmarkHistory$set(file=bookmark$file) # so loaded bookmarks appear in cache list # nolint
    } else {
        firstStep <- app[[ names(app$config$appSteps)[1] ]]        
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
