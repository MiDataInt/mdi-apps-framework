#----------------------------------------------------------------------
# reactive components for constructing a modal panel to cold start an app
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
appChooserServer <- function(
    id
){
    moduleServer(id, function(input, output, session){
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize the module
#----------------------------------------------------------------------
module <- "appChooser"
observers <- list() # for module self-destruction
launchId <- "launchApp"

#----------------------------------------------------------------------
# the table of known apps
#----------------------------------------------------------------------
appDirCols <- list(
    fork = "Fork", 
    suite = "Tool Suite", 
    name = "App Name", 
    description = "Description"
)
tableCols <- c("Action", unlist(appDirCols))
buffer <- NULL
output$table <- renderDT({
        dt <- do.call(rbind, lapply(names(appDirs), function(appName){
            x <- parseAppDirectory(appDirs[[appName]], extended = TRUE)
            if(x$coldStartable) as.data.table(x[names(appDirCols)]) else NULL
        }))
        if(is.null(dt)) stop("No available apps.")
        dt <- dt[][order(fork, suite, name), ]
        buffer <<- dt
        names(dt) <- unlist(appDirCols)
        dt$Action <- tableActionLinks(session$ns(launchId), nrow(dt), "Launch ")
        dt[, ..tableCols]       
    },
    options = list(
        searchDelay = 0
    ),
    class = "display table-compact-4",
    escape = FALSE, 
    selection = "none", 
    editable = FALSE,
    rownames = FALSE,
    server = FALSE
)

#----------------------------------------------------------------------
# respond to a launch button click
#----------------------------------------------------------------------
observers$launch <- observeEvent(input[[launchId]], {
    row <- getTableActionLinkRow(input, launchId)
    loadRequest(list(
        app = buffer[row, name], 
        coldStart = TRUE,
        file = list(name = character(), path = character(),
                    type = character(), nocache = logical()),
        suppressUnlink = TRUE
    ))
})

#----------------------------------------------------------------------
# return value
#----------------------------------------------------------------------
list(
    observers = observers, # for use by destroyModuleObservers
    onDestroy = function() {
        list(  # return the module's cached state object
        )               
    }
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
