#----------------------------------------------------------------------
# reactive components to set pipeline job options
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
configureJobServer <- function(id, options, bookmark, locks){
    moduleServer(id, function(input, output, session){
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        module <- 'configureJob' # for reportProgress tracing
#----------------------------------------------------------------------
if(serverEnv$SUPPRESS_PIPELINE_RUNNER) return(NULL)
dashReplacement <- "DASH"

#----------------------------------------------------------------------
# add tooltips and documentation
#----------------------------------------------------------------------
mdiTooltips(
    session, 
    list(
        c("setName", "Give this configuration set a short, useful name.")
    )
)
addPRDocs('docs', "docs/server-deployment/pipeline-runner", "create-and-edit-job-configuration-files")

#----------------------------------------------------------------------
# initialize job configuration load, create and select elements at top of page
#----------------------------------------------------------------------
sourceFileInput  <- sourceFileInputServer('fileInput', appName = 'pipelineRunner')
jobFileType <- CONSTANTS$sourceFileTypes$jobFile

# initialize the list of related job files (parent table)
jobFileSummaryTemplate <- data.frame(
    Remove      = character(),
    Delete      = character(),
    Suite       = character(),
    Pipeline    = character(),
    FileName    = character(),
    Directory   = character(),
        stringsAsFactors = FALSE
)
jobFiles <- summaryTableServer(
    id = 'jobFiles', # NOT ns(id) when nesting modules!
    parentId = id,
    stepNumber = options$stepNumber,
    stepLocks = locks[[id]],
    sendFeedback = sourceFileInput$sendFeedback,
    template = jobFileSummaryTemplate,
    type = 'shortList',
    remove = list(
        message = paste(
            "Remove this job configuration file from the analysis set?",
            "The associated server file will NOT be deleted."
        ),
        name = 'name'
    ), 
    delete = list(
        message = paste(
            "Permanently delete this job configuration file, and all associated job data, from the server?",
            "This is a serious action that cannot be undone!"
        ),
        name = 'name'
    )
)

# enable cold creation of a new job config file
createFileInput <- createJobFileServer('create', id)

#----------------------------------------------------------------------
# handle an incoming <data>.yml, or a cold start from the launch page #launchPipelineRunner link)
#----------------------------------------------------------------------
loadSourceFile <- function(incomingFile, ...){
    stopSpinner(session, 'loadSourceFile')
    req(incomingFile)
    req(incomingFile$path) 
    startSpinner(session, 'loadSourceFile')    
    reportProgress(incomingFile$path, module)
    yml <- read_yaml(incomingFile$path)
    pipeline <- strsplit(basename(yml$pipeline), ":")[[1]][1]
    suite <- dirname(yml$pipeline)
    jobFiles$list[[incomingFile$path]] <- list(
        name = basename(incomingFile$path),
        directory  = dirname(incomingFile$path),
        path = incomingFile$path,
        suite = if(suite != pipeline) suite else "",
        pipeline = pipeline  
    )
    stopSpinner(session, 'loadSourceFile')
    sourceFileInput$sendFeedback(paste("loaded", incomingFile$path))   
    selectRows(jobFiles$proxy, length(jobFiles$list)) # auto-select the new (last) job file row
}
# add an _additional_ job file uploaded by user via step 1 (not via the launch page)
handleExtraFile <- function(reactive){
    x <- reactive$file()
    req(x)
    loadSourceFile(x)
}
observeEvent(sourceFileInput$file(), {
    handleExtraFile(sourceFileInput)
})
observeEvent(createFileInput$file(), {
    handleExtraFile(createFileInput)
})

#----------------------------------------------------------------------
# reactively update the aggregated jobFiles table
#----------------------------------------------------------------------
observe({
    reportProgress('observe jobFiles$list', module)
    jfs <- jobFileSummaryTemplate
    
    # fill the two tables by source
    nJobFiles <- length(jobFiles$list)
    if(nJobFiles > 0) for(i in 1:nJobFiles){ # whenever the active nJobFiles change
        jobFilePath <- names(jobFiles$list)[i]
        jobFile <- jobFiles$list[[jobFilePath]]
        jfs <- rbind(jfs, data.frame(
            Remove      = "",
            Delete      = "",
            Suite       = jobFile$suite,
            Pipeline    = jobFile$pipeline,            
            FileName    = jobFile$name,
            Directory   = jobFile$directory,
                stringsAsFactors = FALSE
        ))
    }    

    # update the UI reactives
    jobFiles$summary <- jfs
    isolate({
        jobFiles$ids <- names(jobFiles$list)
    })
})

#----------------------------------------------------------------------
# conditional display elements dependent on an active job file selection
#----------------------------------------------------------------------
activeJobFile <- reactive({
    i <- jobFiles$selected()
    req(i)
    jobFiles$list[[i]]
})
observe({
    selectedRow <- jobFiles$selected()
    isSelection <- !is.null(selectedRow) && !is.na(selectedRow)
    toggle(
        selector = "span.requiresJobFile", 
        condition = isSelection
    )
    toggle(
        selector = "div.requiresJobFileMessage", 
        condition = !isSelection
    )
})

#----------------------------------------------------------------------
# load the configuration of a pipeline selected from the jobFiles summary table
#----------------------------------------------------------------------
pipelineConfigs <- list() # cache pipeline configs, they don't change within a session
pipelineConfig <- reactive({
    jobFile <- activeJobFile()
    req(jobFile)
    if(is.null(pipelineConfigs[[jobFile$pipeline]])){
        optionsTable <- getPipelineOptionsTable(jobFile$pipeline) # comprehensive metadata about options        
        template <- getPipelineTemplate(jobFile$pipeline) # ordered actions list and options sets
        pipelineConfigs[[jobFile$pipeline]] <- list(
            actions = template$execute, 
            options = optionsTable,
            template = template
        )
    }
    pipelineConfigs[[jobFile$pipeline]]
})

#----------------------------------------------------------------------
# job file saving and file-path-dependent display elements
#----------------------------------------------------------------------
jobFileValues  <- reactiveValues() # the values present at the last load of the file, prior to save
workingValues  <- reactiveValues() # the values updated with any user changes in UI
pendingValueChanges <- reactiveValues() # whether each file has changes that need to be saved
jobFileActions <- reactiveValues() 
isPendingChanges <- function(path){
    if(is.null(input$actions) || 
       is.null(jobFileActions[[path]]) || 
       is.null(pendingValueChanges[[path]])) return(FALSE)
    length(pendingValueChanges[[path]]) > 0 ||
    !identical(
        sort(input$actions),
        sort(jobFileActions[[path]])
    )
}
reloadInputs <- reactiveVal(0)

# main save button
saveJobFileId <- "saveJobFile"
output$saveJobFileUI <- renderUI({ # dynamically colored button for job file saving
    jobFile <- activeJobFile()
    req(jobFile)
    disabled <- !isPendingChanges(jobFile$path)
    style <- if(disabled) "default" else "success"
    bsButton(ns(saveJobFileId), "Save Job Config", style = style, disabled = disabled)
})

# save as link
saveJobFileAsId <- "saveJobFileAs"
output$saveJobFileAsUI <- renderUI({ # dynamically colored button for job file saving
    jobFile <- activeJobFile()
    req(jobFile)
    serverSaveFileLinkUI(ns(saveJobFileAsId), "Save As...", jobFile$pipeline, ".yml")
})
serverSaveFileButtonServer(saveJobFileAsId, input, session, "yml", 
                           default_type = 'job_default', saveFn = saveJobFileAs)

# style discard changes link based on pending changes
observe({
    jobFile <- activeJobFile()
    disabled <- is.null(jobFile) || !isPendingChanges(jobFile$path)
    toggleClass(
        id = 'discardChanges',
        class = 'pr-link-disable',
        condition = disabled
    )
})

#----------------------------------------------------------------------
# job file main actions
#----------------------------------------------------------------------

# *** job file load action ***
loadJobFile <- function(jobFile){
    req(jobFile)
    path <- jobFile$path
    if(is.null(jobFileValues[[path]])){
        reportProgress(path, 'loading:')
        d <- readDataYml(jobFile)
        jobFileValues[[path]] <- d
        workingValues[[path]] <- d
        pendingValueChanges[[path]] <- list()
    }
}
observeEvent(activeJobFile(), { loadJobFile(activeJobFile()) })

# *** job file save action ***
saveDataYaml <- function(newPath, oldPath){
    jobFile <- activeJobFile()
    config <- pipelineConfig()
    writeDataYml(
        newPath, 
        jobFile$suite,
        jobFile$pipeline, 
        workingValues[[oldPath]],        
        actions = input$actions, 
        optionsTable = config$optionsTable,
        template = config$template
    )
}
observeEvent(input[[saveJobFileId]], {
    path <- activeJobFile()$path
    showUserDialog(
        "Save Configuration Changes", 
        tags$p(paste(
            "Save changes to configuration file?"
        )), 
        tags$p(path),
        callback = function(parentInput) {
            isolate({
                saveDataYaml(path, path)
                jobFileValues[[path]] <- workingValues[[path]]
                jobFileActions[[path]] <- input$actions 
                pendingValueChanges[[path]] <- list()                
            })
        }, 
        type = 'saveCancel',
        size = 'm'
    )
})

# *** job file save as action ***
saveJobFileAs <- function(newPath){
    oldPath <- activeJobFile()$path
    saveDataYaml(newPath, oldPath)
    loadSourceFile(list(path = newPath))
    workingValues[[oldPath]] <- jobFileValues[[oldPath]]
    pendingValueChanges[[oldPath]] <- list()
}

# *** job file revert action, i.e., discard changes ***
observeEvent(input$discardChanges, {
    path <- activeJobFile()$path
    req(isPendingChanges(path))
    showUserDialog(
        "Confirm Discard Changes", 
        tags$p(paste(
            "Discard any changes you have made to the following job configuration file?",
            "The file will be reverted to its previously saved state.",
            "This cannot be undone."
        )), 
        tags$p(path),
        callback = function(parentInput) {
            workingValues[[path]] <- jobFileValues[[path]]
            pendingValueChanges[[path]] <- list()
            reloadInputs( reloadInputs() + 1 )
        }, 
        type = 'discardCancel',
        size = 'm'
    )
})

#----------------------------------------------------------------------
# cascade update pipeline actions to execute (if more than one)    
#----------------------------------------------------------------------
observe({
    config <- pipelineConfig()
    req(config) 
    jobFile <- activeJobFile()
    req(jobFile)
    path <- jobFile$path
    values <- jobFileValues[[path]]
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
getOptionInput <- function(value, option){

    # common components
    id <- paste(unlist(prInputNames), collapse = "_")
    id <- paste('prInput', id, sep = "__")
    id <- gsub("-", dashReplacement, id)
    helpId <- paste(id, "help", sep = "_")
    requiredId <- paste(id, "required", sep = "_")
    dirId <- paste(id, "directory", sep = "_")
    dirId_1 <- paste(dirId, "1", sep = "_") # the first directory in an array updated by shinyFiles widget
    addId <- paste(id, "add", sep = "_")
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
        tags$a(id = ns(addId), class = "pr-add-icon", icon("plus"))
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
    optionFamilyName_no_suite <- rev(strsplit(optionFamilyName, '//')[[1]])[1] # remove any external suite name
    prInputNames$family <<- optionFamilyName_no_suite    
    border <- if(optionFamilyName != rev(optionFamilyNames)[1]) "border-bottom: 1px solid #ddd;" else ""
    fluidRow(
        style = paste("padding: 0 0 10px 0;", border),
        class = if(sum(options$required) == 0) "pr-optional-input" else "",
        lapply(seq_len(nrow(options)), function(i){
            tagList(
                if(i %% 2 == 1) getOptionTag(if(i == 1) optionFamilyName_no_suite else "") else "",
                getOptionTag(options[i, optionName], values[[optionFamilyName_no_suite]], options)
            )
        })
    )
}
output$optionFamilies <- renderUI({
    config <- pipelineConfig()
    req(config) 
    jobFile <- activeJobFile()
    req(jobFile)
    values <- isolate({ workingValues[[jobFile$path]] })
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

# enable toggle for option visibility
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
# watch job inputs for changing values with a single observer
#----------------------------------------------------------------------
observeEvent(input$prInput, {

    # parse the changing value
    path <- activeJobFile()$path    
    x <- gsub(dashReplacement, "-", input$prInput$id)
    x <- strsplit(x, "_")[[1]]
    action <- x[1]
    family <- x[2]
    option <- x[3]
    index  <- as.integer(x[4]) # the array index value

    # commit the new value to workingValues
    new <- input$prInput$value
    workingValues[[path]][[action]][[family]][[option]][index] <- new

    # record whether the new value is different than the _disk_ value
    old <- jobFileValues[[path]][[action]][[family]][[option]][index]
    if(input$prInput$logical){ # since sometimes the disk value is 0/1
        new <- as.logical(new)
        old <- as.logical(old)
    } else {
        new <- as.character(new)
        old <- as.character(old)
    }
    pendingValueChanges[[path]][[input$prInput$id]] <- if(identical(old, new)) NULL else 1
})

#----------------------------------------------------------------------
# define bookmarking actions
#----------------------------------------------------------------------
observe({
    bm <- getModuleBookmark(id, module, bookmark, locks)
    req(bm)
    updateTextInput(session, 'analysisSetName', value = bm$outcomes$analysisSetName)
    jobFiles$list <- bm$outcomes$jobFiles
})

#----------------------------------------------------------------------
# set return values as reactives that will be assigned to app$data[[stepName]]
#----------------------------------------------------------------------
list(
    input = input,
    outcomes = list(
        analysisSetName = reactive(input$analysisSetName),
        jobFiles = reactive(jobFiles$list) # actually a data.frame
    ),
    loadSourceFile = loadSourceFile,
    isReady = reactive({ getStepReadiness(list = jobFiles$list) })
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
