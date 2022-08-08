#----------------------------------------------------------------------
# target app loading; activated by file inputs on launch page
#----------------------------------------------------------------------
loadRequest <- reactiveVal(list())
retryLoadRequest <- reactiveVal(0)
appApprovalFile <- file.path(serverEnv$DATA_DIR, "mdi-app-approval.yml")
executeLoadRequest <- function(loadRequest){ # act on an approved app load request
    startSpinner(session, 'executeLoadRequest') 

    # authorize the requested app
    NAME <- loadRequest$app
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
    gitStatusData$app$name <- NAME
    gitStatusData$app$version <- if(is.null(app$config$version)) "na" else app$config$version
    gitStatusData$suite$dir <- R.utils::getAbsolutePath( file.path(app$DIRECTORY, '..', '..', '..') )
    gitStatusData$suite$name <- basename(gitStatusData$suite$dir)
    gitStatusData$suite$versions <- getAllVersions(gitStatusData$suite$dir)
    gitStatusData$suite$head <- getGitHead(gitStatusData$suite)

    # TODO: check working version, bookmark version, latest version, etc.
    # offer user the option to switch to matching legacy or latest version, after checking for breaking changes
    # if bookmark being loaded, check bookmark versions against latest (and working if different)
    # if date file being loaded (i.e, without prior version info), check working against latest

    # load all relevant session scripts in reverse precedence order
    #   global, then session, folders were previously sourced by initializeSession.R on page load
    sessionEnv$sourceLoadType <- "app"
    loadSuccess <- loadAllRScripts(app$sources$suiteGlobalDir, recursive = TRUE)
    if(!loadSuccess) return(NULL)
    loadSuccess <- loadAppScriptDirectory(app$sources$suiteSessionDir)
    if(!loadSuccess) return(NULL)
    loadSuccess <- loadAppScriptDirectory(DIRECTORY) # add all scripts defined within the app itself; highest precedence
    if(!loadSuccess) return(NULL)
    sessionEnv$sourceLoadType <- ""

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

    # # enable developer interface in private modes only
    # if(!serverEnv$IS_SERVER && serverEnv$IS_DEVELOPER) {
    #     loadAppScriptDirectory('developer')
    #     addDeveloperMenuItem()
    # }

    # determine the best way to initialize the UI for this user and incoming file
    nAppSteps <- length(app$config$appSteps)
    appStepNames <- names(app$config$appSteps)
    userFirstVisit <- is.null(cookie) || is.null(cookie[[app$NAME]]) || cookie[[app$NAME]] != 'true'
    isBookmarkFile <- loadRequest$file$type == "bookmark"
    showSplashScreen <- !isBookmarkFile && (nAppSteps == 0 || (userFirstVisit && !serverEnv$IS_DEVELOPER))      
    splashScreenName <- 'appName' # the name of the app overview tab
    selectedStep <- if(showSplashScreen) splashScreenName else {
        path <- loadRequest$file$path
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
    if(!serverEnv$IS_LOCAL) serverBookmark <<- bookmarkingServer('saveBookmarkToServer', list(shinyFiles = TRUE), locks)

    # load servers for all required appStep modules, plus finally run appServer
    # because this is the slowest initialization step, defer many until after first UI load
    if(!exists('appServer')) appServer <- function() NULL # for apps with no specific server code
    runModuleServers <- function(startI, endI){
        lapply(startI:endI, function(i){
            stepName <- names(app$config$appSteps)[i]
            reportProgress(paste('loadStepModuleServers', stepName))            
            step <- app$config$appSteps[[i]]
            server <- get(paste0(step$module, 'Server'))
            if(is.null(step$options)) step$options <- list()
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
    
    # enable git repository status in sidebar
    insertUI(".main-sidebar", where = "beforeEnd", immediate = TRUE,   
        ui = {
            id <- 'gitStatus'
            sibebarGitStatusServer(id)            
            sibebarGitStatusUI(id)
        }
    )

    # push the initial file upload to the app via it's first step module
    if(loadRequest$file$type == CONSTANTS$sourceFileTypes$bookmark){
        bookmark$file <- loadRequest$file$path
        nocache <- loadRequest$file$nocache
        if(is.null(nocache) || !nocache) bookmarkHistory$set(file=bookmark$file) # so loaded bookmarks appear in cache list # nolint
    } else {
        firstStep <- app[[ names(app$config$appSteps)[1] ]]        
        firstStep$loadSourceFile(loadRequest$file, suppressUnlink = loadRequest$suppressUnlink)
    } 

    # clean up
    stopSpinner(session, 'executeLoadRequest')
    observeLoadRequest$destroy() # user has to reload page to reset to launch page once an app is loaded
    session$sendCustomMessage('setDocumentCookie', list(
        name  = app$NAME,
        data  = list(value = TRUE, isServerMode = serverEnv$IS_SERVER),
        nDays = 365
    ))    
}
observeLoadRequest <- observeEvent({ # handle a request to load an app
    loadRequest()
    retryLoadRequest()
}, {
    loadRequest <- loadRequest()
    req(loadRequest$app)
    app_ <- parseAppSuite(appDirs[[loadRequest$app]])
    appKey <- paste(app_$suite, app_$name, sep = " / ")
    appApprovals <- if(file.exists(appApprovalFile)) read_yaml(appApprovalFile) else list()
    if(is.null(appApprovals[[appKey]]) && # get approval to load an app for the first local/remote use
      !serverEnv$IS_SERVER                # public server apps are implicitly approved by the maintainer
    ){
        showUserDialog(                                         
            "Approve App for Use", 
            tags$p(paste(
                "MDI apps can access the file system and execute commands on your computer.",
                "It is essential that you trust the people who develop the apps you run."
            )), 
            tags$p("For more information about MDI security, see:"), 
            tags$p(tags$a(
                style = "margin-left: 2em;",
                href = "https://midataint.github.io/docs/registry/00_index/", 
                target = "Docs",
                "MDI Tool Suite Registry"
            )),
            tags$p(tags$a(
                style = "margin-left: 2em;",
                href = "https://midataint.github.io/mdi-desktop-app/docs/security-notes.html", 
                target = "Docs",
                "MDI Desktop Security Notes"
            )),
            tags$p("Click 'OK' to confirm that you understand and accept the risks of using the following app on your computer or server:"),
            tags$p(
                style = "margin-left: 2em; font-weight: bold;",
                appKey
            ), 
            callback = function(...) {
                appApprovals[[appKey]] <<- TRUE
                write_yaml(appApprovals, appApprovalFile)
                executeLoadRequest(loadRequest)
            },
            size = "m", 
            type = 'okCancel'
        )
    } else { # skip prompt for previously approved apps
        executeLoadRequest(loadRequest)
    }
})
