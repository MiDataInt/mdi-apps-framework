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
    pipeline <- basename(yml$pipeline)
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

        # use 'mdi <pipeline> template' to recover the ordered actions list and options sets
        args <- c(jobFile$pipeline, 'template', "--all") 
        template <- runMdiCommand(args)
        req(template$success)
        template <- read_yaml(text = template$results)
        actions <- template$execute

        # use 'mdi <pipeline> optionsTable' to recover comprehensive information about options
        # does NOT include default values yet
        args <- c(jobFile$pipeline, 'optionsTable')
        optionsTable <- runMdiCommand(args)
        req(optionsTable$success)
        optionsTable <- fread(text = optionsTable$results)
        optionsTable$required <- as.logical(optionsTable$required) 

        # return the results
        pipelineConfigs[[jobFile$pipeline]] <- list(
            actions = actions, 
            options = optionsTable,
            template = template
        )
    }
    pipelineConfigs[[jobFile$pipeline]]
})

#----------------------------------------------------------------------
# job file saving and file-path-dependent display elements
#----------------------------------------------------------------------
jobFileValues <- reactiveValues() # the values present at the last load of the file, prior to save
workingValues <- reactiveValues() # the values updated with any user changes in UI

# main save button
saveJobFileId <- "saveJobFile"
output$saveJobFileUI <- renderUI({ # dynamically colored button for job file saving
    jobFile <- activeJobFile()
    req(jobFile)
    buttonType <- "success" # TODO: update with check for changes
    serverSaveFileButtonUI(ns(saveJobFileId), "Save Job Config", jobFile$name, ".yml", 
                           buttonType = buttonType)
})
serverSaveFileButtonServer(saveJobFileId, input, session, "yml", 
                           default_type = 'job_default', saveFn = saveJobFile)

# save as link
saveJobFileAsId <- "saveJobFileAs"
output$saveJobFileAsUI <- renderUI({ # dynamically colored button for job file saving
    jobFile <- activeJobFile()
    req(jobFile)
    serverSaveFileLinkUI(ns(saveJobFileAsId), "Save As...", jobFile$pipeline, ".yml")
})
serverSaveFileButtonServer(saveJobFileAsId, input, session, "yml", 
                           default_type = 'job_default', saveFn = saveJobFileAs)

# #----------------------------------------------------------------------
# # job file main actions
# #----------------------------------------------------------------------

# *** job file load action ***
loadJobFile <- function(jobFile){
    req(jobFile)
    id <- jobFile$path
    if(is.null(jobFileValues[[id]])){
        reportProgress(id, 'loading:')
        d <- readDataYml(jobFile)
        jobFileValues[[id]] <- d
    }
}
observeEvent(activeJobFile(), { loadJobFile(activeJobFile()) })

# *** job file save action ***
saveJobFile <- function(jobFilePath){
    write_yaml(parseDataYml(), file = jobFilePath)
}

# *** job file save as action ***
saveJobFileAs <- function(jobFilePath){
    write_yaml(parseDataYml(), file = jobFilePath)
    loadSourceFile(list(path = jobFilePath))
}

# *** job file revert action, i.e., discard changes ***
observeEvent(input$discardChanges, {
    path <- activeJobFile()$path
    showUserDialog(
        "Confirm Discard Changes", 
        tags$p(paste(
            "Discard any changes you have made to the following job configuration file?",
            "The file will be reverted to its previously saved state.",
            "This cannot be undone."
        )), 
        tags$p(path),
        callback = function(parentInput) {
            NULL # TODO: finish this, may want to suppress deletion of executed jobs?
        }, 
        type = 'discardCancel',
        size = "m"
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
    values <- jobFileValues[[jobFile$path]]
    req(values)
    updateCheckboxGroupInput(
        'actions',
        session  = session,
        choices  = config$actions,
        selected = values$execute,
        inline = TRUE
    )   
    toggle('actionSelectors', condition = length(config$actions) > 1)  
})

#----------------------------------------------------------------------
# cascade update panels to enter/adjust job options by family
#----------------------------------------------------------------------
getOptionInput <- function(optionName, option){
    if(requiredOnly() && !option$required) return("")
    id <- ns(paste('input', optionName, sep = "_"))
    requiredId <- paste(id, "required", sep = "_")
    helpId <- paste(id, "help", sep = "_")
    placeholder <- paste(option$type, if(option$required) "REQUIRED" else "")
    label <- HTML(paste(
        optionName, 
        if(option$required) tags$span(id = requiredId, class = "pr-required-icon", icon("asterisk")) else "",
        tags$span(id = helpId, class = "pr-help-icon", icon("question"))
    ))
    input <- if(option$type == "boolean") 
        checkboxGroupInput(id, label, choices = "", 
                           selected = if(as.logical(as.integer(option$default))) "" else NULL)
    else 
        textInput(id, label, value = option$default, placeholder = placeholder)
    tagList(
        input,
        bsTooltip(helpId, option$description, placement = "top"),
        if(option$required) bsTooltip(requiredId, "required", placement = "top") else "",
    )
}
getOptionTag <- function(option, options = NULL){
    isLabel <- is.null(options)
    column(
        width = if(isLabel) 2 else 3,
        style = if(isLabel) "margin-top: 20px;" else "margin-top: 10px;",
        if(isLabel) tags$p(tags$strong(
            option
        )) else getOptionInput(
            option,
            options[optionName == option]
        )
    )
}
getOptionFamilyTags <- function(optionFamilyName, options, optionFamilyNames){
    options <- options[optionFamily == optionFamilyName]
    if(requiredOnly() && sum(options$required) == 0) return("")    
    border <- if(optionFamilyName != rev(optionFamilyNames)[1]) "border-bottom: 1px solid #ddd;" else ""
    fluidRow(
        style = paste("padding: 0 0 10px 0;", border),
        lapply(seq_len(nrow(options)), function(i){
            tagList(
                if(i %% 3 == 1) getOptionTag(if(i == 1) optionFamilyName else "") else "",
                getOptionTag(options[i, optionName], options)
            )
        })
    )
}
output$optionFamilies <- renderUI({
    config <- pipelineConfig()
    req(config) 
    jobFile <- activeJobFile()
    req(jobFile)
    values <- jobFileValues[[jobFile$path]]
    req(values)
    startSpinner(session, "output$optionFamilies")
    tabActions <- if(length(config$actions) > 1) input$actions else config$actions
    tabs <- lapply(tabActions, function(actionName){
        options <- config$options[action == actionName][order(universal, optionFamily, order, -required)]
        optionFamilyNames <- options[, unique(optionFamily)]
        tabPanel(
            actionName, 
            tags$div(
                style = "padding-left: 15px;",
                lapply(optionFamilyNames, getOptionFamilyTags, options, optionFamilyNames)
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
    toggle('showRequiredOnly', condition = !requiredOnly())
    toggle('showAllOptions',   condition =  requiredOnly())
})
observeEvent(input$showRequiredOnly, { requiredOnly(TRUE) })
observeEvent(input$showAllOptions,   { requiredOnly(FALSE) })

#----------------------------------------------------------------------
# define bookmarking actions
#----------------------------------------------------------------------
observe({
    bm <- getModuleBookmark(id, module, bookmark, locks)
    req(bm)
    jobFiles$list <- bm$outcomes$jobFiles
})

#----------------------------------------------------------------------
# set return values as reactives that will be assigned to app$data[[stepName]]
#----------------------------------------------------------------------
list(
    outcomes = list(
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
