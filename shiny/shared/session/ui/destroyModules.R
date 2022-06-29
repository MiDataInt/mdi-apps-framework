#----------------------------------------------------------------------
# these functions help clean up values and observers when a module is 
# is removed from the UI, e.g., using removeUI() or removeModal(),
# which only remove elements from the client DOM (NOT server inputs and observers)
#----------------------------------------------------------------------
# see mdi-apps-framework/shiny/shared/session/modules/widgets/framework/commandTerminal
# for a complete working example
#----------------------------------------------------------------------
# removeMatchingInputValues() prevents zombie inputs from consuming memory
# and allows the same module instance id to be reused to load a module multiple times
# without the problems that arise from stale values from the previous instance
#----------------------------------------------------------------------
removeMatchingInputValues <- function(session, id){ # note that session$ns(id) is called here
    .values <- .subset2(session$input, "impl")$.values # .subset2 essentially equivalent to [[
    keys <- .values$keys()
    isOurs <- startsWith(keys, session$ns(id))
    sapply(keys[isOurs], .values$remove) # thus, the VALUE of the input is purged
}
#----------------------------------------------------------------------
# destroyModuleObservers() allows interested modules to optionally:
#   1 - declare a list of observers that should be destroyed when the module is removed from the UI
#   2 - declare a function, onDestroy(), that is executed and optionally returns a state value
#       e.g., to allow the next load to be initialized to the same state as the last load
#----------------------------------------------------------------------
destroyModuleObservers <- function(moduleObject){ # the module object returned by myModuleServer(id, ...)
    if(!is.null(moduleObject$observers)) lapply(moduleObject$observers, function(x) x$destroy())
    if(!is.null(moduleObject$onDestroy)) moduleObject$onDestroy() else NULL
}
