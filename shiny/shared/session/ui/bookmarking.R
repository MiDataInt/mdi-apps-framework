# help construct the proper "Save Your Work" links based on serverEnv
saveYourWorkLinks <- function(){

    # in server mode, show two buttons, one for local download, one for shinyFiles
    if(serverEnv$IS_SERVER){
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
            bookmarkingUI(
                'saveBookmarkToServer', 
                list(
                    label = "to the Server", 
                    shinyFiles = TRUE
                )
            )
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
