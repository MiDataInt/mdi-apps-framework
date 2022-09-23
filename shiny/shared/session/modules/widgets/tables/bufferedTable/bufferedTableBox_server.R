#----------------------------------------------------------------------
# server components for the bufferedTableBox widget module
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
bufferedTableBoxServer <- function(
    id,
    ..., # arguments and UI elements passed to activateMdiHeaderLinks() and bufferedTableServer()
    url = NULL,
    reload = NULL,
    baseDirs = NULL,
    envir = NULL,
    dir = NULL,
    download = NULL,
    settings = NULL 
) { 
    moduleServer(id, function(input, output, session) {    
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize module
#----------------------------------------------------------------------
module <- 'bufferedTableBox'
settings <- activateMdiHeaderLinks( # uncomment as needed
    session,
    ...,
    url = url,
    reload = reload,
    baseDirs = baseDirs,
    envir = envir,
    dir = dir,
    download = download,
    settings = settings
)

#----------------------------------------------------------------------
# return the bufferedTable
#----------------------------------------------------------------------
table <- bufferedTableServer(
    "buffered",
    parentId = id,
    parentInput = input,
    ...
)

#----------------------------------------------------------------------
# set return value, typically NULL or a list of reactives
#----------------------------------------------------------------------
list(
    settings = settings,
    table = table
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
