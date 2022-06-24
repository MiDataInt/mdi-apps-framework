#----------------------------------------------------------------------
# reactive components for text editing a job configuration file
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
jobFileInputEditorServer <- function(id, editMode, activeJobFile, reloadInputs){
    moduleServer(id, function(input, output, session){
        module <- 'jobFileInputEditor' # for reportProgress tracing
        ns <- session$ns
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize the editor
#----------------------------------------------------------------------
dashReplacement <- "DASH"
jobFileType <- CONSTANTS$sourceFileTypes$jobFile
state <- list(
    disk    = reactiveValues(), # the file contents present at the last load of the file, prior to save
    working = reactiveValues(), # the file contents updated with any user changes in script editor
    pending = reactiveValues()  # whether each file has changes that need to be saved 
)
isInputs <- reactive({
    req(editMode())
    editMode() == "inputs"
})
pipelineConfigs <- list() # cache pipeline configs, they don't change within a session
pipelineConfig <- reactive({
    req(isInputs())
    jobFile <- activeJobFile()
    req(jobFile)
    if(is.null(pipelineConfigs[[jobFile$pipeline]])){
        optionsTable <- getPipelineOptionsTable(jobFile$pipeline) # comprehensive metadata about options        
        template <- getPipelineTemplate(jobFile$pipeline) # ordered actions list and options sets
        pipelineConfigs[[jobFile$pipeline]] <- list(
            actions  = template$execute, 
            options  = optionsTable,
            template = template
        )
    }
    pipelineConfigs[[jobFile$pipeline]]
})
jobFileActions <- reactiveValues() 

#----------------------------------------------------------------------
# job file loading
#----------------------------------------------------------------------
observe({ 
    req(isInputs())
    jobFile <- activeJobFile()
    req(jobFile)
    path <- jobFile$path
    if(is.null(state$disk[[path]])){
        startSpinner(session, "loadJobFile")        
        reportProgress(path, 'loading:')
        d <- readDataYml(jobFile)
        state$disk[[path]] <- d
        state$working[[path]] <- d
        state$pending[[path]] <- list()
        stopSpinner(session, "loadJobFile")
    }
})

#----------------------------------------------------------------------
# cascade update pipeline actions to execute (if more than one)    
#----------------------------------------------------------------------
observe({
    req(isInputs())
    config <- pipelineConfig()
    req(config) 
    jobFile <- activeJobFile()
    req(jobFile)
    path <- jobFile$path
    values <- state$disk[[path]]
    req(values)
    actions <- values$execute
    jobFileActions[[path]] <- actions
    reloadInputs()
    updateCheckboxGroupInput(
        'actions',
        session  = session,
        choices  = config$actions,
        selected = actions,
        inline = TRUE
    )   
    toggle('actionSelectors', condition = length(config$actions) > 1)  
})

#----------------------------------------------------------------------
# cascade update panels to enter/adjust job options by family
#----------------------------------------------------------------------
prInputNames <- list(
    action = "",
    family = "",
    option = ""
)
prInputFamilyNames <- list()
getOptionInput <- function(value, option){

    # common components
    id <- paste(unlist(prInputNames[c('action', 'option')]), collapse = "_") # options names must be unique in an action
    prInputFamilyNames[[id]] <<- prInputNames$family # but keep track of the families used to organize options
    id <- paste('prInput', id, sep = "__")
    id <- gsub("-", dashReplacement, id)
    helpId <- paste(id, "help", sep = "_")
    requiredId <- paste(id, "required", sep = "_")
    dirId <- paste(id, "directory", sep = "_")
    dirId_1 <- paste(dirId, "1", sep = "_") # the first directory in an array updated by shinyFiles widget
    addId <- paste(id, "add", sep = "_")
    removeId <- paste(id, "remove", sep = "_")
    isDirectory <- endsWith(prInputNames$option, "-dir") || grepl("-dir-", prInputNames$option)
    placeholder <- paste(
        if(isDirectory) "directory" else option$type, 
        if(option$required) "REQUIRED" else ""
    )
    label <- HTML(paste(
        prInputNames$option, 
        tags$span(id = ns(helpId), class = "mdi-help-icon", icon("question")),        
        if(option$required) tags$span(id = ns(requiredId), class = "pr-required-icon", icon("asterisk")) else "",
        if(isDirectory) {
            serverChooseDirIconServer(dirId_1, input, session, chooseFn = handleChooseDir)
            serverChooseDirIconUI(ns(dirId_1)) 
        } else "",
        tags$a(id = ns(addId), class = "pr-add-icon", icon("plus"), 
               href = paste0("javascript:prAddToList('", addId, "')")),
        tags$a(id = ns(removeId), class = "pr-add-icon", icon("minus"), 
               href = paste0("javascript:prRemoveLastItem('", removeId, "')"))
    ))

    # custom inputs with a single tracking function/event
    x <- if(option$type == "boolean") 
        mdiCheckboxGroupInput(ns(id), label, value, onchangeFn = "prCheckboxOnChange")
    else if(option$type == "integer") 
        mdiIntegerInput(ns(id), label, value, placeholder, onchangeFn = "prInputOnChange")
    else if(option$type == "double") 
        mdiDoubleInput(ns(id), label, value, placeholder, onchangeFn = "prInputOnChange")
    else   
        mdiTextInput(ns(id), label, value, placeholder, onchangeFn = "prInputOnChange")

    # input with tooltip
    tags$span(
        class = if(option$required) "" else "pr-optional-input",
        x,
        mdiTooltip(session, helpId, option$description, ui = TRUE),
        # if(isDirectory) mdiTooltip(session, dirId_1, "click to search for a directory", ui = TRUE),
        # mdiTooltip(session, addId, "add an array item", ui = TRUE),
        # if(option$required) bsTooltip(requiredId, "required", placement = "top") else "",
    )
}
getOptionTag <- function(option, values = NULL, options = NULL){
    prInputNames$option <<- option
    isLabel <- is.null(options)
    column(
        width = if(isLabel) 2 else 5,
        style = if(isLabel) "margin-top: 20px;" else "margin-top: 10px;",
        if(isLabel) tags$p(tags$strong(
            option
        )) else getOptionInput(
            values[[option]],
            options[optionName == option]
        )
    )
}
getOptionFamilyTags <- function(optionFamilyName, values, options, optionFamilyNames){
    options <- options[optionFamily == optionFamilyName]
    prInputNames$family <<- optionFamilyName    
    border <- if(optionFamilyName != rev(optionFamilyNames)[1]) "border-bottom: 1px solid #ddd;" else ""
    fluidRow(
        style = paste("padding: 0 0 10px 0;", border),
        class = if(sum(options$required) == 0) "pr-optional-input" else "",
        lapply(seq_len(nrow(options)), function(i){
            tagList(
                # left side short-form family name labels
                if(i %% 2 == 1) getOptionTag(if(i == 1) rev(strsplit(optionFamilyName, '//')[[1]])[1] else "") else "",
                # right side option inputs
                getOptionTag(options[i, optionName], values[[optionFamilyName]], options)
            )
        })
    )
}
output$optionFamilies <- renderUI({
    req(isInputs())
    config <- pipelineConfig()
    req(config) 
    jobFile <- activeJobFile()
    req(jobFile)
    values <- isolate({ state$working[[jobFile$path]] })
    req(values)
    reloadInputs()
    startSpinner(session, "output$optionFamilies")
    tabActions <- if(length(config$actions) > 1) input$actions else config$actions
    tabs <- lapply(tabActions, function(actionName){
        prInputNames$action <<- actionName
        options <- config$options[action == actionName][order(universal, familyOrder, order, -required, optionName)]
        optionFamilyNames <- options[, unique(optionFamily)]
        tabPanel(
            actionName, 
            tags$div(
                style = "padding-left: 15px;",
                lapply(optionFamilyNames, getOptionFamilyTags, 
                       values[[actionName]], options, optionFamilyNames)
            )
        )
    })
    tabs$id <- "pipelineRunnerOptionTabs"
    tabs$width <- 12
    stopSpinner(session, "output$optionFamilies")
    do.call(tabBox, tabs)
})

#----------------------------------------------------------------------
# enable toggle for option visibility
#----------------------------------------------------------------------
requiredOnly <- reactiveVal(FALSE)
observeEvent(requiredOnly(), { 
    reqOnly <- requiredOnly()
    toggle('showRequiredOnly', condition = !reqOnly)
    toggle('showAllOptions',   condition =  reqOnly)
    toggle(selector = ".pr-optional-input", condition = !reqOnly)
})
observeEvent(input$showRequiredOnly, { requiredOnly(TRUE) })
observeEvent(input$showAllOptions,   { requiredOnly(FALSE) })

#----------------------------------------------------------------------
# handle shinyFile selection of a directory being set into an option
#----------------------------------------------------------------------
handleChooseDir <- function(x){
    x$id <- gsub("_directory", "", x$id)
    updateTextInput(session, x$id, value = x$dir)
}

#----------------------------------------------------------------------
# handle list item additions and contractions
#----------------------------------------------------------------------
parsePrInput <- function(id, indexed = FALSE){
    d <- list(path = activeJobFile()$path)
    x <- gsub(dashReplacement, "-", id)
    x <- strsplit(x, "_")[[1]]
    d$action <- x[1]
    d$option <- x[2]
    if(indexed) d$index <- as.integer(x[3])
    d$family <- prInputFamilyNames[[paste(d$action, d$option, sep = "_")]]
    d$current <- state$working[[d$path]][[d$action]][[d$family]][[d$option]]
    d$nItems <- length(d$current)
    d
}
getPrInputId <- function(id, index){
    parts <- strsplit(id, '_')[[1]]
    prefix <- paste(parts[1:(length(parts) - 1)], collapse = '_')
    parts[length(parts)] <- index
    id <- paste(parts, collapse = '_')
    list(
        prefix = prefix,
        index = index,
        id = id,
        cssId = session$ns(paste0("prInput__", id))
    )
}
setListPending <- function(x, prefix){
    old <- state$disk[[x$path]][[x$action]][[x$family]][[x$option]]
    new <- state$working[[x$path]][[x$action]][[x$family]][[x$option]]
    maxI <- max(length(old), length(new)) 
    for(i in seq_len(maxI)){
        id <- paste(prefix, i, sep = "_")
        new_ <- new[i]        
        old_ <- old[i]
        if(is.logical(old_)){
            new_ <- as.logical(new_)
            old_ <- as.logical(old_)
        } else {
            new_ <- as.character(new_)
            old_ <- as.character(old_)
        }
        state$pending[[x$path]][[id]] <- if(identical(old_, new_)) NULL else 1
    }
    for(id in names(state$pending[[x$path]])){
        if(!startsWith(id, paste0(prefix, "_"))) next
        i <- as.integer(rev(strsplit(id, "_")[[1]])[1])
        if(i > maxI) state$pending[[x$path]][[id]] <- NULL
    }
}
observeEvent(input$prAddToList, {
    x <- parsePrInput(input$prAddToList)
    state$working[[x$path]][[x$action]][[x$family]][[x$option]] <- c(x$current, x$current[x$nItems])
    id    <- getPrInputId(input$prAddToList, x$nItems)
    newId <- getPrInputId(input$prAddToList, x$nItems + 1)
    session$sendCustomMessage("prDuplicateLastInput", list(id = id$cssId, newId = newId$cssId))
    setListPending(x, id$prefix)
})
observeEvent(input$prRemoveLastItem, {
    x <- parsePrInput(input$prRemoveLastItem)
    if(x$nItems > 1){
        state$working[[x$path]][[x$action]][[x$family]][[x$option]] <- x$current[1:(x$nItems - 1)]
        id <- getPrInputId(input$prRemoveLastItem, x$nItems)
        session$sendCustomMessage("prRemoveLastInput", id$cssId)
        setListPending(x, id$prefix)
    }
})

#----------------------------------------------------------------------
# watch job inputs for changing values with a single observer
#----------------------------------------------------------------------
observeEvent(input$prInput, {
    req(isInputs())

    # parse and commit the new value to workingValues
    x <- parsePrInput(input$prInput$id, indexed = TRUE)
    new <- input$prInput$value
    state$working[[x$path]][[x$action]][[x$family]][[x$option]][x$index] <- new

    # record whether the new value is different than the _disk_ value
    old <- state$disk[[x$path]][[x$action]][[x$family]][[x$option]][x$index]
    if(input$prInput$logical){ # since sometimes the disk value is 0/1
        new <- as.logical(new)
        old <- as.logical(old)
    } else {
        new <- as.character(new)
        old <- as.character(old)
    }
    state$pending[[x$path]][[input$prInput$id]] <- if(identical(old, new)) NULL else 1
})

#----------------------------------------------------------------------
# return value
#----------------------------------------------------------------------
list(
    state = state,
    pending = function(path) {
        if(is.null(input$actions) || 
           is.null(jobFileActions[[path]]) || 
           is.null(state$pending[[path]])) return(FALSE)
        length(state$pending[[path]]) > 0 ||
        !identical(
            sort(input$actions),
            sort(jobFileActions[[path]])
        )   
    },
    write = function(newPath, oldPath){
        jobFile <- activeJobFile()
        config <- pipelineConfig()
        writeDataYml(
            newPath, 
            jobFile$suite,
            jobFile$pipeline, 
            state$working[[oldPath]], # retains suite//option name format       
            actions = input$actions, 
            optionsTable = config$options,
            template = config$template
        )
    },
    save = function(path){
        state$disk[[path]] <- state$working[[path]]
        jobFileActions[[path]] <- input$actions 
        state$pending[[path]] <- list()  
    },
    saveAs = function(newPath, oldPath){
        state$working[[oldPath]] <- state$disk[[oldPath]]
        state$pending[[oldPath]] <- list()
    },
    discard = function(path){
        state$working[[path]] <- state$disk[[path]]
        state$pending[[path]] <- list()
        reloadInputs( reloadInputs() + 1 )
    }
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
