#----------------------------------------------------------------------
# reactive components for creating a button to load a previously loaded data package
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
priorPackagesServer <- function( # generally, you do not call priorPackagesServer directly
    id                          # see showpriorPackages()
){
    moduleServer(id, function(input, output, session){
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize the module
#----------------------------------------------------------------------
module <- "priorPackages"
observers <- list() # for module self-destruction
dataPackages <- reactiveVal( scanDataPackages(session) )
selectedRow <- reactiveVal(NULL)

#----------------------------------------------------------------------
# construct the interactive display table of server data packages
#----------------------------------------------------------------------
dataPackagesTable <- reactive({
    dataPackages()[, .(
        Pipeline = pipelineLong,
        Action = action,
        "Data Name" = dataName,
        Date = mtime
    )]
})
packageTable <- bufferedTableServer(
    "packageTable",
    id,
    input,
    tableData = dataPackagesTable,
    selection = 'single',
    selectionFn = selectedRow,
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
            dataPackages = dataPackages(),
            selectedRow = selectedRow()
        )               
    }
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
