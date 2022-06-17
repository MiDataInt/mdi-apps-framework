#----------------------------------------------------------------------
# reactive components for link to load an MDI docs page in a new browser tab
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
documentationLinkServer <- function(
    id, 
    gitUser = "MiDataInt", # the GitHub user or organization
    repository = NULL, # the base repository name, if not "midataint.github.io"
    docPath, # the relative file path to the documentation target, e.g., "path/to/docs.html" (".html" is optional)
    anchor = NULL # the name of an optional heading anchor on the page, e.g., "first-heading"
) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
#----------------------------------------------------------------------

# show a tooltip on the icon
mdiTooltip(session, "show", "Open the documentation page")

# parse the docs url and open in a new browser tab
gitUser <- tolower(gitUser)
observeEvent(input$show, {
    url <- paste0(
        "https://", 
        gitUser, 
        ".github.io/", 
        if(is.null(repository)) "" else paste0(repository, "/"),
        docPath, 
        if(is.null(anchor)) "" else paste0("#", anchor)
    )
    js <- paste0(
        "window.open('", 
        url,
        "', '_mdi_docs');"
    )
    runjs(js)
})

#----------------------------------------------------------------------
# return nothing
#----------------------------------------------------------------------
NULL

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
