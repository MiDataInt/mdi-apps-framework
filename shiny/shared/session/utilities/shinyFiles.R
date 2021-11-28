#----------------------------------------------------------------------
# wrapper functions for shinyFiles access to server files
# enforces authorization
#----------------------------------------------------------------------
# note: this is not a module due to the implementation of the shinyFiles package
#----------------------------------------------------------------------

serverFilesButtonUI <- function(id){
    shinyFilesButton(
        id,
        "Browse on Server",
        "Select data package, bookmark, or other source file to import",
        multiple = FALSE,
        buttonType = "default",
        class = NULL,
        icon = NULL,
        style = NULL,
        viewtype = "detail"
    )
}
serverFilesButtonServer <- function(id, input, session){
    volumes <- c(TEST = "/srv/mdi")
    observe({
        message()
        message("input[[id]]")
        print(input[[id]])
    })
    shinyFileChoose(
        input,
        id,
        updateFreq = 0,
        session = session,
        defaultRoot = NULL,
        defaultPath = ""
    )
}
