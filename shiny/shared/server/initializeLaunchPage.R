#----------------------------------------------------------------------
# initialize launch page to allow data import from upload file or host drive
#----------------------------------------------------------------------
if(!restricted){
    
    # initial file upload (just one initial file, additional files added later via sourceFileUpload module)
    output$mainFileInputUI <- renderUI({
        id <- 'mainFileInput'
        sourceFileInputServer(id)
        sourceFileInputUI(id)
    })

    # user status, dataDir and logout in navbar / page header
    insertUI(".navbar-static-top",  where = "beforeEnd", immediate = TRUE,   
        ui = {
            id <- 'headerStatus'
            headerStatusServer(id)
            headerStatusUI(id)
        }
    )
    
    # bookmarks cached on user's local computer
    bookmarkHistory <- NULL
    output$bookmarkHistoryList <- renderUI({
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
    })
}
