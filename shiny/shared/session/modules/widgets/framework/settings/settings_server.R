#----------------------------------------------------------------------
# reactive components for caching and only occasionally displaying a set 
# of input parameters for controlling how an application step or component behaves
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
    resettable = TRUE, # if TRUE, a Reset All Setting link will be provided
    presets = list(),  # a named list of available settings presets, applied on top of defaults, as list(Preset_Name = list(tab = list(option = x)))
    s3Class = NULL # optional S3 class to assign to the settings object
) {
    moduleServer(id, function(input, output, session) {
        module <- 'settings' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize module
#----------------------------------------------------------------------
gearId <- 'gearIcon'
fullGearId <- paste(parentId, id, gearId, sep = "-")
dialogObservers <- reactiveValues()

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
    if(length(t) == 0) return()
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
hasValidSettings <- length(template) > 0
if(hasValidSettings) initializeTemplate(template)
else reportProgress(paste("CONFIG ERROR:", parentId, id, "had no valid settings templates"))

# settings values
settings <- reactiveValues()
allSettings <- reactiveVal()
previousState <- list() # the most recent settings state prior to the current one, to support "undo"
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
    previousState <<- allSettings()
    allSettings(x)
    setCachedValues(x)
}
initializeSettings(template)

#----------------------------------------------------------------------
# react to user click of gear icon by opening a modal popup
#----------------------------------------------------------------------
resetAllSettingsId <- paste(parentId, id, "resetAllSettings", sep = "-")
nPresets <- length(presets)
isPresets <- nPresets > 0
presetIds <- if(isPresets) paste(parentId, id, names(presets), sep = "-") else NULL
showSettingsModals <- function(){ # open the modal panel with all settings
    req(hasValidSettings, nTabs > 0)
    showUserDialog(
        title,
        toInputs(),
        if(resettable) actionLink(resetAllSettingsId, "Reset All Settings") else "",
        if(isPresets) tagList(
            span("Presets: ", style = "margin-left: 5px;"),
            lapply(1:nPresets, function(i) span(actionLink(presetIds[i], names(presets)[i]), style = "margin-left: 5px;") )
        ) else "",
        size = workingSize,        
        callback = fromInputs,
        fade = fade,
        type = if(immediate) "okOnly" else "okCancel",
        observers = dialogObservers
    )    
}
observeEvent(input[[gearId]], { showSettingsModals() }) # NOT a dialog observer, resides in parent element
setAllSettings <- function(preset = NULL){
    lapply(names(template), function(tab){
        lapply(names(settings[[tab]]), function(id){
            t <- template[[tab]][[id]]
            if(!is.null(preset) && !is.null(preset[[tab]][[id]])){
                t$value <- if(is.list(preset[[tab]][[id]])) preset[[tab]][[id]]$value else preset[[tab]][[id]]
            }
            fullId <- session$ns(id)
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
}
if(resettable) observeEvent(sessionInput[[resetAllSettingsId]], { 
    setAllSettings() 
})
if(isPresets) for(i in 1:nPresets) observeEvent(sessionInput[[presetIds[i]]], {
    setAllSettings(presets[[i]]) 
})

#----------------------------------------------------------------------
# getter and setter functions
#----------------------------------------------------------------------

# generate lists of inputs in a tabbed panel for all requested settingss
getTabInputs <- function(id, tab){
    x <- settings[[tab]][[id]]
    t <- template[[tab]][[id]]
    t$label <- gsub('_', ' ', id)
    fullId <- session$ns(id)
    if(immediate) dialogObservers[[fullId]] <- observeEvent(sessionInput[[fullId]], setValue(tab, id, fullId))        
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
        dataSource = dataSourceSelect(fullId, t, x),
        spacer = span(style = "visibility: hidden;", textInput(fullId, fullId, "")),
        get(x$type)(fullId, t$label, x$value)
    ), style = "margin-bottom: 5px; min-height: 60px;")    
}
toInputs <- function(){
    if(isTabbed){
        fluidRow(do.call(tabBox, c(
            lapply(names(template), function(tab){            
                hasSettings <- length(settings[[tab]]) > 0 && 
                               any(sapply(names(settings[[tab]]), function(id) template[[tab]][[id]]$type != "spacer"))
                if(hasSettings) tabPanel(
                    fluidRow(lapply(names(settings[[tab]]), getTabInputs, tab)),
                    title = gsub('_', ' ', tab)
                ) else NULL
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
    dialogObservers[[buttonId]] <- observeEvent(sessionInput[[buttonId]], {
        file <- sessionInput[[buttonId]]
        file.copy(file$datapath, filePath(file$name))
        updateTextInput(sessionSession, fullId, value = file$name)
    })
    dialogObservers[[clearId]] <- observeEvent(sessionInput[[clearId]], {
        unlink(filePath(sessionInput[[fullId]]))        
        updateTextInput(sessionSession, fullId, value = "")
    })
    tagList(
        fileInput(buttonId, t$label, accept = t$accept),
        disabled(textInput(fullId, NULL, x$value)),
        actionLink(clearId, "Remove File")
    )
}
dataSourceSelect <- function(fullId, t, x){
    upload <- app[[ appStepNamesByType$upload ]]
    sources <- upload$outcomes$sources()
    sourceIds <- names(sources)
    names(sourceIds) <- sapply(sources, function(x) x$unique$Project[1])
    selectInput(fullId, t$label, choices = sourceIds)
}

# update our cached setting values when user commits changes from the modal
setValue <- function(tab, id, fullId){ # in immediate mode
    settings[[tab]][[id]]$value <- sessionInput[[fullId]] 
    x <- reactiveValuesToList(settings)
    previousState <<- allSettings()
    allSettings(x)
    setCachedValues(x)
}
setValues <- function(id, tab, input){ # in delayed mode 
    settings[[tab]][[id]]$value <- input[[session$ns(id)]] 
}
fromInputs <- function(input){ # same as from bookmark
    lapply(names(template), function(tab){
        lapply(names(settings[[tab]]), setValues, tab, input)
    })
    x <- reactiveValuesToList(settings)
    previousState <<- allSettings()
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
retval$get <- function(tab, id, default = NULL){
    x <- settings[[tab]]
    if(is.null(x)) return(default)
    x <- x[[id]]
    if(is.null(x) || is.null(x$value)) return(default)
    x$value
}
retval$set <- function(tab, id, value){
    settings[[tab]][[id]]$value <- value
}
retval$setChoices <- function(tab, id, choices){
    template[[tab]][[id]]$choices <<- choices
}
retval$open <- showSettingsModals
retval$undo <- function(){
    initializeSettings(previousState)
}
structure(
    retval,
    class = c(s3Class, "mdiSettings")
) 

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
