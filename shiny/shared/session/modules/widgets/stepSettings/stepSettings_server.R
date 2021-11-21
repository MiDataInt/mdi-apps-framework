#----------------------------------------------------------------------
# reactive components for caching and only occasionally displaying a set 
# of input parameters for controlling how an application step behaves
#----------------------------------------------------------------------
# user click of a gear icon opens a dynamically populated popup
#----------------------------------------------------------------------
# settings are read from module.yml; see other modules for format examples
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
stepSettingsServer <- function(
    id, parentId, 
    size=NULL,
    cacheKey=NULL # a reactive/reactiveVal that returns an id for the current settings state
) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        parentNs <- NS(parentId)
        module <- 'stepSettings' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize module
#----------------------------------------------------------------------
gearId <- 'gearIcon'
fullGearId <- paste(parentId, id, gearId, sep = "-")

# setting values cache, for pages where settings change in response to calls to 'replace'
cache <- list()
getCachedValues <- function(){
    if(is.null(cacheKey)) return( list() )
    x <- cacheKey()
    if(is.null(x) || is.na(x)) return( list() )
    d <- cache[[x]]
    if(!is.list(d)) return( list() )
    d
}
setCachedValues <- function(d){
    if(is.null(cacheKey)) return()
    x <- cacheKey()
    if(is.null(x) || is.na(x)) return()
    cache[[x]] <<- d
} 

# settings template
template <- stepModuleInfo[[ app$info$appSteps[[parentId]]$module ]]$settings
if(is.null(template)) template <- list()
nTabs <- 1
isTabbed <- FALSE
initializeTemplate <- function(t){
    template <<- t
    nTabs <<- length(t) # template forces these, not any override coming from a potentially stale bookmark
    isTabbed <<- nTabs > 1
    toggle(
        id = fullGearId, 
        asis = TRUE, # does not work consistently to let shinyjs handle id resolution (not sure why)
        condition = nTabs > 0
    )
}
initializeTemplate(template)

# settings values
settings <- reactiveValues()
allSettings <- reactiveVal()
initializeSettings <- function(init=NULL, newTemplate=NULL){ # executed as function to allow bookmark recovery
    if(is.null(init)) init <- getCachedValues()
    if(!is.null(newTemplate)) initializeTemplate(newTemplate)
    x <- lapply(names(template), function(tab) { # process coerces incoming bookmark to match the current template
        if(is.null(init[[tab]])) init[[tab]] <- template[[tab]]
        y <- lapply(names(template[[tab]]), function(param){
            if(is.null(init[[tab]][[param]])) template[[tab]][[param]] else init[[tab]][[param]] 
        })
        names(y) <- names(template[[tab]])
        settings[[tab]] <- y
        y
    })
    names(x) <- names(template)
    allSettings(x)
    setCachedValues(x)
}
initializeSettings(template)

#----------------------------------------------------------------------
# react to user click of gear icon by opening a modal popup
#----------------------------------------------------------------------
observeEvent(input[[gearId]], {
    req(nTabs > 0)
    showUserDialog(
        "Set Parameters",
        toInputs(),
        size = if(!is.null(size)) size else if(nTabs <= 3) 's' else if(nTabs <= 6) 'm' else 'l',        
        callback = fromInputs
    )
})

#----------------------------------------------------------------------
# getter and setter functions
#----------------------------------------------------------------------

# generate lists of inputs in a tabbed panel for all requested settingss
getTabInputs <- function(id, tab){
    x <- settings[[tab]][[id]]
    t <- template[[tab]][[id]]
    t$label <- gsub('_', ' ', id)
    id <- parentNs(id)
    getOption <- function(name, default=NA) if(is.null(x[[name]])) default else x[[name]]
    div(switch(
        t$type,
        selectInput = selectInput(id, t$label, choices = t$choices, selected = x$value),
        numericInput = numericInput(id, t$label, x$value, getOption('min'), getOption('max'), getOption('step')),
        get(x$type)(id, t$label, x$value)
    ), style = "margin-bottom: 5px;")          
}
toInputs <- function(){
    if(isTabbed){
        fluidRow(do.call(tabBox, c(
            lapply(names(settings), function(tab){
                do.call(tabPanel, c(
                    lapply(names(settings[[tab]]), getTabInputs, tab),
                    title = gsub('_', ' ', tab)
                ))
            }),
            width = 12               
        )))        
    } else {
        tab1 <- names(settings)[1]
        fluidRow(do.call(column, c(
            lapply(names(settings[[tab1]]), getTabInputs, tab1),
            width = 12
        )))
    }
}

# update our cached setting values when user commits changes from the modal
setValues <- function(id, tab, input){
    settings[[tab]][[id]]$value <- input[[parentNs(id)]] 
}
fromInputs <- function(input){ # same as from bookmark
    lapply(names(settings), function(tab){
        lapply(names(settings[[tab]]), setValues, tab, input)
    })
    x <- reactiveValuesToList(settings)
    allSettings(x)
    setCachedValues(x)
}

#----------------------------------------------------------------------
# set return value; one named member of list for each tab, plus all_
#----------------------------------------------------------------------
retval <- reactiveValuesToListOfReactives(settings) # the categorized settings reactives
retval$all_ <- reactive({ allSettings() })
retval$replace <- initializeSettings # called when a bookmark is loaded to replace settings en bloc
retval$cache <- reactive({ cache })
retval$get <- function(type, name){
    x <- allSettings()[[type]]
    if(is.null(x)) return(NULL)
    x <- x[[name]]
    if(is.null(x)) return(NULL)
    x$value
}
retval

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
