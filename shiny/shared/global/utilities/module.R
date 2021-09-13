#----------------------------------------------------------------------
# convert a reactiveValues object IN a module to a list of reactives to be return BY a module
# this refactoring allows (i) addition of other reactives and (ii) proper bookmarking
#----------------------------------------------------------------------
# in module:
#     data <- reactiveValues(a=1)
#     # observe and set data$a in module code as desired
#     retval <- reactiveValuesToListOfReactives(data)
#     retval$xyz <- reactive({ input$xyz })
#     retval$fn  <- function(...){  }
#     return(retval)
# in parent:
#     instance <- module()
#     observe(instance$a, {})  # <<< reactive
#----------------------------------------------------------------------
reactiveValuesToListOfReactives <- function(rv){
    retval <- lapply(names(rv), function(name) reactive({ rv[[name]] }) )
    names(retval) <- names(rv)
    retval
}

#----------------------------------------------------------------------
# convert a list coming from a bookmark to a reactiveValues object
#----------------------------------------------------------------------
listToReactiveValues <- function(x){
    rv <- reactiveValues()
    for(name in names(x)) rv[[name]] <- x[[name]]   
    rv
}