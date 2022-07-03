#----------------------------------------------------------------------
# initialize launch page to allow data import from upload file or host drive
#----------------------------------------------------------------------
if(!restricted){
    
    # initial file upload (just one initial file, additional files added later via sourceFileUpload module)
    if(isAuthorizedUser()) output$mainFileInputUI <- renderUI({
        id <- 'mainFileInput'
        sourceFileInputServer(id)
        sourceFileInputUI(id)
    })

    # user status, dataDir and logout in navbar / page header
    insertUI(".navbar-static-top .sidebar-toggle", where = "afterEnd", immediate = TRUE,   
        ui = tags$i(
            id = "mainSpinner",
            class = "fas fa-spinner fa-spin header-large-icon",
            style = "margin-left: 0.5em; font-size: 1.35em; color: #eee; visibility: hidden;"
        )
    )
    if(serverEnv$IS_DEVELOPER){
        insertUI(".navbar-static-top .sidebar-toggle", where = "afterEnd", immediate = TRUE,   
            ui = {
                id <- 'reloadAppScripts'
                reloadAppScriptsServer(id)
                reloadAppScriptsUI(id)
            }
        )
    }    
    if(checkConfigEditPermission()){
        insertUI(".navbar-static-top .sidebar-toggle", where = "afterEnd", immediate = TRUE,   
            ui = {
                id <- 'configEditorLink'
                configEditorLinkServer(id)
                configEditorLinkUI(id)
            }
        )
    }
    insertUI(".navbar-static-top .sidebar-toggle", where = "afterEnd", immediate = TRUE,   
        ui = {
            id <- 'stage2-apps-docs'
            documentationLinkServer(id, url = "https://midataint.github.io/docs/usage/#stage-2-apps")
            documentationLinkUI(id, isAppHeader = TRUE)
        }
    )
    
    # user status, dataDir and logout in navbar / page header
    headerStatusId <- 'headerStatus'
    headerStatus <- headerStatusServer(headerStatusId)
    insertUI(".navbar-static-top", where = "beforeEnd", immediate = TRUE,   
        ui = {
            headerStatusUI(headerStatusId)
        }
    )

    # bookmarks cached on user's local computer
    bookmarkHistory <- NULL
    output$bookmarkHistoryList <- if(isAuthorizedUser()) renderUI({
        id <- 'bookmarkHistory'
        bookmarkHistory <<- bookmarkHistoryServer(id)

        # reset page to most recent bookmark when top left logo is clicked
        if(!is.null(queryString$resetPage)) observeEvent(bookmarkHistory$list$table(), {
            req(nrow(bookmarkHistory$list$table()) > 0)
            hash <- bookmarkHistory$list$table()[1, hash]
            bookmark <- bookmarkHistory$list$get(hash = hash)
            loadBookmarkFromString(bookmark)
        })       

        bookmarkHistoryUI(id)

    # message to communicate authorization failure (despite authentication success)
    # piggybacks into bookmarkHistoryList for convenience
    }) else renderUI({
        tagList(
            hr(),
            tags$p(
                tags$strong("You are not authorized to use the resources on this server."),
                style = "color: rgb(125,0,0); font-size: 1.05em;"
            ),
            tags$p(
                "Please contact the site administrator if you think you have reached this message in error."
            )
        )
    })

    # enable the Pipeline Runner app from a cold start link, i.e., with no uploaded input file
    observeEvent(input$launchPipelineRunner, {
        loadRequest(list(
            app = "pipelineRunner",
            file = list(
                type = "",
                path = ""
            ),
            suppressUnlink = NA
        ))
    })
}
