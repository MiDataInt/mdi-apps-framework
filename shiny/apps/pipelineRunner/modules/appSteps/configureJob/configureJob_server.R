#----------------------------------------------------------------------
# reactive components to set pipeline job options
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
configureJobServer <- function(id, options, bookmark, locks) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        module <- 'configureJob' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize installed pipelines
#----------------------------------------------------------------------
pipelineSuiteDirs <-  getPipelineSuiteDirs()
pipelineDirs <- getPipelineDirs(pipelineSuiteDirs)
pipelineSuites <- getInstalledPipelineSuites(pipelineDirs)

#----------------------------------------------------------------------
# cascade update suite and pipeline selectInputs
#----------------------------------------------------------------------
isolate({
    updateSelectInput('suite', choices = pipelineSuites, session = session)
})
observeEvent(input$suite, {
    req(input$suite)
    updateSelectInput('pipeline', choices = getInstalledPipelines(input$suite), session = session)
})
pipelineConfig <- reactive({
    req(input$suite)
    req(input$pipeline)  

    # use 'mdi <pipeline> template' to recover the ordered actions list
    args <- c(input$pipeline, 'template') 
    template <- runMdiCommand(args)
    req(template$success)
    template <- read_yaml(text = template$results)
    actions <- template$execute

    # use 'mdi <pipeline> optionsTable' to recover comprehensive information about options
    # does NOT include default values yet
    args <- c(input$pipeline, 'optionsTable')
    optionsTable <- runMdiCommand(args)
    req(optionsTable$success)
    optionsTable <- fread(text = optionsTable$results)
    optionsTable$required <- as.logical(optionsTable$required) 

    # return the results
    list(
        actions = actions, 
        options = optionsTable,
        template = template
    )
})
suiteName <- reactive({
    req(input$suite)
    rev(strsplit(input$suite, '/')[[1]])[1]
})

#----------------------------------------------------------------------
# control job file saving and file-path-dependent display elements
#----------------------------------------------------------------------
jobFile <- reactiveVal(NULL) # current working job file, the main output of this module
jobFileValues <- reactiveVal(list())

# control job file UI elements
output$jobFilePath <- renderUI({  # display jobFile path to user, with delete link
    req(jobFile())
    tags$p(
        jobFile(), 
        " ",
        tags$span(style = "margin-left: 1em;"), actionLink(ns("deleteJobFile"), "Delete")
    )
})
output$saveJobFileUI <- renderUI({ # dynamically colored button for job file saving
    req(input$pipeline)
    buttonType <- if(is.null(jobFile())) "success" else "default"
    serverSaveFileButtonUI(ns("saveJobFile"), "Save Job Config", input$pipeline, ".yml", 
                           buttonType = buttonType)
})

# *** job file load action ***
observeEvent(jobFile(), {
    req(jobFile())
    jobFileValues( readDataYml(jobFile()) )
    disable('suite') # once a file is saved, the pipeline cannot be changed
    disable('pipeline')
})

# *** job file save action ***
saveJobFile <- function(jobFilePath){
    write_yaml(parseDataYml(), file = jobFilePath)
    jobFile(jobFilePath)
}
serverSaveFileButtonServer("saveJobFile", input, session, "yml", 
                           default_type = 'job_default', saveFn = saveJobFile)

# *** job file delete action ***
observeEvent(input$deleteJobFile, {
    showUserDialog(
        "Confirm Job File Deletion", 
        tags$p("The following job file will be permanently deleted. This cannot be undone."), 
        tags$p(jobFile()),
        callback = function(parentInput) NULL, # TODO: finish this, may want to suppress deletion of executed jobs?
        type = 'okCancel',
        size = "m"
    )
})

# conditional display elements dependent on job file path
# important because option default values may depend on file path context, e.g., <pipeline>.yml
observe({
    toggle(
        selector = "span.requiresJobFile", 
        condition = !is.null(jobFile())
    )
    toggle(
        selector = "div.requiresJobFileMessage", 
        condition = is.null(jobFile())
    )
})

#----------------------------------------------------------------------
# handle an incoming job.yml, or a cold start from the launch page #launchPipelineRunner link
#----------------------------------------------------------------------
loadSourceFile <- function(incomingFile, ...){
    stopSpinner(session)    
    req(incomingFile)
    req(is.character(incomingFile))
    jobFile(incomingFile)
}

#----------------------------------------------------------------------
# cascade update pipeline actions to execute (if more than one)    
#----------------------------------------------------------------------
observe({
    config <- pipelineConfig()
    values <- jobFileValues()
    req(config)
    req(values)
    updateCheckboxGroupInput(
        'actions',
        session  = session,
        choices  = config$actions,
        selected = config$actions,
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
    values <- jobFileValues()
    req(config)
    req(config$actions)    
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
# convert input option values to job yml (for writing) and vice versa (for loading)
#----------------------------------------------------------------------

# use 'mdi <pipeline> valuesTable' to recover the context-dependent job values from <data>.yml
readDataYml <- function(jobFile){
    args <- c('valuesTable', 'valuesTable', jobFile)
    valuesTable <- runMdiCommand(args)
    req(valuesTable$success)
    read_yaml(text = valuesTable$results)
}

# parse inputs to a partial <data>.yml file
parseDataYml <- function(){
    config <- pipelineConfig()
    req(config)  

    # first save, include all actions to start
    if(is.null(jobFile())) return(config$template)

    # saving after option value changes, requested actions only
    req(input$actions)

    # get the option values for each action
    x <- lapply(c("_PIPELINE_", input$actions), function(actionName){

        dmsg(actionName)

        if(actionName == "_PIPELINE_") return( paste(suiteName(), input$pipeline, sep = "/") )

        options <- config$options[action == actionName]
        optionFamilyNames <- options[, unique(optionFamily)]
        x <- lapply(optionFamilyNames, function(optionFamilyName){
            options <- options[optionFamily == optionFamilyName]
            x <- lapply(seq_len(nrow(options)), function(i){
                option <- options[i]

                dmsg(option$optionName)


                value <- {
                    id <- paste('input', option$optionName, sep = "_")
                    value <- input[[id]]

                    if(option$type == "boolean") value <- if(is.null(value)) 0 else 1

dprint(value)

                    if(option$required || value != option$default) value else NULL
                }
                if(!is.null(value) && value == "_NA_") value <- "NA" 
                value
            })
            names(x) <- options$optionName
            x
        })
        names(x) <- optionFamilyNames
        x
    })
    names(x) <- c("pipeline", input$actions)
    x$execute <- input$actions
    x
}

#----------------------------------------------------------------------
# define bookmarking actions
#----------------------------------------------------------------------
observe({
    bm <- getModuleBookmark(id, module, bookmark, locks)
    req(bm)
    jobFile(bm$outcomes$jobFile)
    # updateTextInput(session, 'analysisSetName', value = bm$outcomes$analysisSetName)
    # sources$list  <- bm$outcomes$sources
    # samples$list  <- bm$outcomes$samples
    # samples$names <- bm$outcomes$sampleNames
})

#----------------------------------------------------------------------
# set return values as reactives that will be assigned to app$data[[stepName]]
#----------------------------------------------------------------------
list(
    outcomes = list(
        jobFile = jobFile
    ),
    loadSourceFile = loadSourceFile,
    isReady = reactive({ 
        getStepReadiness(fn = function() !is.null(jobFile()) ) 
    })
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
