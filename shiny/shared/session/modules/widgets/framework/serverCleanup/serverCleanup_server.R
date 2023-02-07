#----------------------------------------------------------------------
# reactive components for constructing an interface to delete dataDir folders
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
serverCleanupServer <- function( # generally, you do not call serverCleanupServer directly
    id                          # see showServerCleanup()
){
    moduleServer(id, function(input, output, session){
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize the module
#----------------------------------------------------------------------
module <- "serverCleanup"
spinnerSelector <- "#serverCleanupSpinner"
observers <- list() # for module self-destruction
dataPackages <- reactiveVal( scanDataPackages(session) )

#----------------------------------------------------------------------
# handle package deletion
#----------------------------------------------------------------------
selectedPackages <- integer()
handleSelectDelete <- function(d){    
    selectedPackages <<- if(d$newValue) unique(c(selectedPackages, d$selectedRow))
                         else selectedPackages[selectedPackages != d$selectedRow]    
    d
}
observers$deleteSelectedPackages <- observeEvent(input$deleteSelectedPackages, {
    req(selectedPackages)
    show(selector = spinnerSelector)
    x <- dataPackages()
    packageDirs <- x[selectedPackages, packageDir]
    unlink(packageDirs, recursive = TRUE)
    dataPackages(x[!(packageDir %in% packageDirs)])
    hide(selector = spinnerSelector)
})

#----------------------------------------------------------------------
# construct the interactive display table of server data packages
#----------------------------------------------------------------------
dataPackagesTable <- reactive({
    dataPackages()[, .(
        Delete = tableCheckboxes(session$ns("selectDelete"), rep(FALSE, .N)),
        Pipeline = pipelineLong,
        Action = action,
        "Data Name" = dataName,
        Replicates = replicates,
        Date = mtime
    )]
})
packageTable <- bufferedTableServer(
    "packageTable",
    id,
    input,
    tableData = dataPackagesTable,
    editBoxes = list(
        selectDelete = list(
            type = 'checkbox',
            handler = handleSelectDelete,
            session = session
        )
    ),
    selection = 'none',
    options = list(
        searchDelay = 0
    )
)

#----------------------------------------------------------------------
# return value
#----------------------------------------------------------------------
dataPackages # required, otherwise lazy evaluation won't propagate value into onDestroy
list(
    observers = observers, # for use by destroyModuleObservers
    onDestroy = function() {
        list(  # return the module's cached state object
            dataPackages = dataPackages()
        )               
    }
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
