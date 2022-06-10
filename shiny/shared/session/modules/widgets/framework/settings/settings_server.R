#----------------------------------------------------------------------
# reactive components for caching and only occasionally displaying a set 
# of input parameters for controlling how an application step or component behaves
#----------------------------------------------------------------------
# user click of a gear icon opens a dynamically populated popup
#----------------------------------------------------------------------
# settings are read from module.yml, settings.yml or as a list
# see bottom and other modules for format examples
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
settingsServer <- function(
    id, 
    parentId, 
    templates = list(parentId), # list of one or more of: parentId, a path to settings.yml, or a matching list
    size = NULL,
    cacheKey = NULL, # a reactive/reactiveVal that returns an id for the current settings state
    fade = FALSE,
    title = "Set Parameters",
    immediate = FALSE, # if TRUE, setting changes are transmitted in real time
    resettable = TRUE  # if TRUE, a Reset All Setting link will be provided
) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        parentNs <- NS(parentId)
        module <- 'settings' # for reportProgress tracing
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
template <- list() # one or more template sources to concatenate in order
if(!is.null(templates)) for(t in templates){
    template <- c(template, if(is.character(t)){
        if(!is.null(app$config$appSteps[[t]])){ # appStep settings
            stepModuleInfo[[ app$config$appSteps[[t]]$module ]]$settings
        } else if(file.exists(t)){ # component module settings
            read_yaml(t)
        } else { # bad call, proceed with no template
            reportProgress(paste("failed to load settings template:", t))
            NULL
        }
    } else t) # caller-provided pre-assembled options list
}
nTabs <- 1
isTabbed <- FALSE
workingSize <- size
inputWidth <- "s"
initializeTemplate <- function(t){
    template <<- t
    nTabs <<- length(t) # template forces these, not any override coming from a potentially stale bookmark
    isTabbed <<- nTabs > 1
    maxTabSize <- max(sapply(template, length))
    workingSize <<- if(!is.null(size)) size 
               else if(nTabs >= 6 || maxTabSize > 16) 'l' 
               else if(nTabs >= 3 || maxTabSize > 8) 'm' else 's'
    inputWidth <<- if(workingSize == "l") 4 else if(workingSize == "m") 6 else 12
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
initializeSettings <- function(init = NULL, newTemplate = NULL){ # executed as function to allow bookmark recovery
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
resetAllSettingsId <- paste(parentId, id, "resetAllSettings", sep = "-")
observeEvent(input[[gearId]], {
    req(nTabs > 0)
    showUserDialog(
        title,
        toInputs(),
        if(resettable) actionLink(resetAllSettingsId, "Reset All Settings") else "",
        size = workingSize,        
        callback = fromInputs,
        fade = fade,
        type = if(immediate) "okOnly" else "okCancel"
    )
})
if(resettable) observeEvent(sessionInput[[resetAllSettingsId]], {
    lapply(names(template), function(tab){
        lapply(names(settings[[tab]]), function(id){
            t <- template[[tab]][[id]]
            fullId <- parentNs(id)
            switch(
                t$type,
                textInput = updateTextInput(sessionSession, fullId, value = t$value),
                numericInput = updateNumericInput(sessionSession, fullId, value = t$value),        
                selectInput = updateSelectInput(sessionSession, fullId, selected = t$value),
                radioButtons = updateRadioButtons(sessionSession, fullId, selected = t$value),
                checkboxGroupInput = updateCheckboxGroupInput(sessionSession, fullId, selected = t$value),
                checkboxInput = updateCheckboxInput(sessionSession, fullId, value = t$value)
            )
        })
    })
})

#----------------------------------------------------------------------
# getter and setter functions
#----------------------------------------------------------------------

# generate lists of inputs in a tabbed panel for all requested settingss
getTabInputs <- function(id, tab){
    x <- settings[[tab]][[id]]
    t <- template[[tab]][[id]]
    t$label <- gsub('_', ' ', id)
    fullId <- parentNs(id)
    if(immediate) observeEvent(sessionInput[[fullId]], setValue(tab, id, fullId))        
    getOption <- function(name, default=NA) if(is.null(x[[name]])) default else x[[name]]
    getInline <- function() if(!is.null(t$inline)) t$inline else TRUE
    column(width = inputWidth, switch(
        t$type,
        numericInput = numericInput(
            fullId, 
            t$label, 
            x$value, 
            getOption('min'), 
            getOption('max'), 
            getOption('step')
        ),        
        selectInput = selectInput(
            fullId, 
            t$label, 
            choices = t$choices, 
            selected = x$value
        ),
        radioButtons = radioButtons(
            fullId, 
            t$label, 
            choices = t$choices, 
            selected = x$value,
            inline = getInline()
        ),
        checkboxGroupInput = checkboxGroupInput(
            fullId, 
            t$label, 
            choices = t$choices, 
            selected = x$value,
            inline = getInline()
        ),
        fileInput = fileInputPanel(fullId, t, x),
        spacer = span(style = "visibility: hidden;", textInput(fullId, fullId, "")),
        get(x$type)(fullId, t$label, x$value)
    ), style = "margin-bottom: 5px;")    
}
toInputs <- function(){
    if(isTabbed){
        fluidRow(do.call(tabBox, c(
            lapply(names(template), function(tab){
                tabPanel(
                    fluidRow(lapply(names(settings[[tab]]), getTabInputs, tab)),
                    title = gsub('_', ' ', tab)
                )
            }),
            width = 12               
        )))        
    } else {
        tab1 <- names(template)[1]
        fluidRow(do.call(column, c(
            lapply(names(settings[[tab1]]), getTabInputs, tab1),
            width = 12
        )))
    }
}

# composite inputs for complex actions like file uploads
fileInputPanel <- function(fullId, t, x){
    buttonId <- paste(fullId, "button", sep = "-")
    clearId  <- paste(fullId, "clear",  sep = "-")
    filePath <- function(fileName) file.path(serverEnv$UPLOADS_DIR, fileName)
    observeEvent(sessionInput[[buttonId]], {
        file <- sessionInput[[buttonId]]
        file.copy(file$datapath, filePath(file$name))
        updateTextInput(sessionSession, fullId, value = file$name)
    })
    observeEvent(sessionInput[[clearId]], {
        unlink(filePath(sessionInput[[fullId]]))        
        updateTextInput(sessionSession, fullId, value = "")
    })
    tagList(
        fileInput(buttonId, t$label, accept = t$accept),
        disabled(textInput(fullId, NULL, x$value)),
        actionLink(clearId, "Remove File")
    )
}

# update our cached setting values when user commits changes from the modal
setValue <- function(tab, id, fullId){ # in immediate mode
    settings[[tab]][[id]]$value <- sessionInput[[fullId]] 
    x <- reactiveValuesToList(settings)
    allSettings(x)
    setCachedValues(x)
}
setValues <- function(id, tab, input){ # in delayed mode
    settings[[tab]][[id]]$value <- input[[parentNs(id)]] 
}
fromInputs <- function(input){ # same as from bookmark
    lapply(names(template), function(tab){
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
retval$get <- function(tab, id){
    x <- settings[[tab]]
    if(is.null(x)) return(NULL)
    x <- x[[id]]
    if(is.null(x)) return(NULL)
    x$value
}
retval

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------

# legacy name assignment
stepSettingsServer <- settingsServer

#----------------------------------------------------------------------
# settings: use camel case and '_' in names (it is replaced with a space in the UI)
#----------------------------------------------------------------------
# settings:
#     Tab_Name_1:
#         Setting_Name_1:
#             type:   textInput
#             value:  "some text"
#         Setting_Name_2:
#             type:   numericInput
#             min:    1
#             max:    4
#             step:   1
#             value:  2
#     Tab_Name_2: 
#         Setting_Name_3:
#             type:   selectInput
#             choices:
#                 - xxx
#                 - yyy
#             value:  xxx  
#         Setting_Name_4:
#             type:   radioButtons
#             choices:
#                 - xxx
#                 - yyy
#             value: xxx
#             inline: true 
#         Setting_Name_5:
#             type:   checkboxGroupInput
#             choices:
#                 - xxx
#                 - yyy
#                 - zzz
#             value: 
#                 - xxx
#                 - yyy
#             inline: true 
