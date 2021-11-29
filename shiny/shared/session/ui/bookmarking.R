# help construct the proper "Save Your Work" links based on serverEnv
saveYourWorkLinks <- function(){

    # in server mode, show two buttons, one for local download, one for shinyFiles
    if(serverEnv$IS_SERVER){
        addServerBookmarkObserver()
        tagList(
            tags$p(
                "Save Your Work",
                style = "margin: 2em 0 0.25em 1em; color: #dddddd;"  
            ),
            bookmarkingUI(
                'saveBookmarkFile',     
                list(
                    label = "to Your Computer"
                )
            ),
            getServerBookmarkingUI() # see below, is dynamic
        )

    # otherwise, just show one button for local download
    } else {
        bookmarkingUI(
            'saveBookmarkFile', 
            list(
                label = "Save Your Work", 
                class = "sidebarBookmarking sidebarBookmarkingSingle"
            )
        )
    }
}
serverBookmarkingContainer <- "serverBookmarkingContainer"
serverBookmarkingContainerId <- paste0("#", serverBookmarkingContainer)
getServerBookmarkingUI <- function(filename){
    tags$span( 
        id = serverBookmarkingContainer,
        bookmarkingUI(
            'saveBookmarkToServer', 
            list(
                label = "to the Server", 
                shinyFiles = TRUE,
                filename = filename
            )
        )
    )
}

# watch appStep 1 analysisSetName to update the server default file name
addServerBookmarkObserver <- function(){
    observe({
        firstStepName <- names(app$config$appSteps)[1]
        analysisSetName <- app[[firstStepName]]$outcomes$analysisSetName()
        req(analysisSetName)
        isolate({
            filename <- getDefaultBookmarkName(suppressExtension = TRUE)
            removeUI(paste(serverBookmarkingContainerId, '*'), multiple = TRUE, immediate = TRUE)
            insertUI(serverBookmarkingContainerId, where = "afterBegin", immediate = TRUE, 
                     ui = getServerBookmarkingUI( filename ) )
        })
    })
}

# contruct a nice, informative, default name for all bookmarks
getDefaultBookmarkName <- function(suppressExtension = FALSE){
    firstStepName <- names(app$config$appSteps)[1]
    appName <- app$config$name
    analysisSetName <- app[[firstStepName]]$outcomes$analysisSetName()
    filename <- paste(appName, analysisSetName, input$sidebarMenu, sep = ".")
    filename <- gsub(' ', '_', filename)
    if(!suppressExtension) filename <- paste(filename, "mdi", sep = ".")
    filename
}
