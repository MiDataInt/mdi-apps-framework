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
    docPath = NULL, # the relative file path to the documentation target, e.g., "path/to/docs.html" (".html" is optional)
    anchor = NULL, # the name of an optional heading anchor on the page, e.g., "first-heading"
    url = NULL # use this web address, ignoring all other values
) {
    moduleServer(id, function(input, output, session) {
#----------------------------------------------------------------------

# show a tooltip on the icon
mdiTooltip(session, "show", "Open the documentation")

# parse the docs url
if(is.null(url)) url <- paste0(
    "https://", 
    tolower(as.character(gitUser)), 
    ".github.io/", 
    if(is.null(repository)) "" else paste0(repository, "/"),
    docPath, 
    if(is.null(anchor)) "" else paste0("#", anchor)
)
open <- paste0("window.open('", url,"', 'Docs');")
blur <- paste0("document.getElementById('", session$ns('show'),"').blur();")

# act on use icon click
observeEvent(input$show, {
    if(is.null(serverEnv$MDI_IS_ELECTRON)){ # open in standard web browser tab
        runjs(paste(open, blur))
    } else { # open in electron tab
        session$sendCustomMessage("frameworkToElectron", list(
            type = "showDocumentation", 
            data = url
        ))
    }
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
