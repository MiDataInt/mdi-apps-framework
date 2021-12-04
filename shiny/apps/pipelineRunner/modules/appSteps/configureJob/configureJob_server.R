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
    args <- c(input$pipeline, 'template', '--all-options')
    template <- runMdiCommand(args)
    req(template$success)
    template <- read_yaml(text = template$results)
    actions <- template$execute
    template <- template[names(template) %in% actions]
    list(
        actions = actions, 
        optionFamilies = template # keyed by pipeline action
    )
})

#----------------------------------------------------------------------
# control job file saving and file-path-dependent display elements
#----------------------------------------------------------------------
jobFile <- reactiveVal(NULL) # current working job file, the main output of this module

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
observeEvent(jobFile(), {
    req(jobFile())
    disable('suite') # once a file is saved, the pipeline cannot be changed
    disable('pipeline')
})

# job file save action
saveJobFile <- function(jobFilePath){
    jobFile(jobFilePath)
}
serverSaveFileButtonServer("saveJobFile", input, session, "yml", 
                           default_type = 'job_default', saveFn = saveJobFile)

# job file delete action
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
# cascade update pipeline actions to execute (if more than one)    
#----------------------------------------------------------------------
observe({
    config <- pipelineConfig()
    req(config)
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
getOptionTag <- function(option, options = NULL){
    isLabel <- is.null(options)
    column(
        width = if(isLabel) 2 else 3,
        style = if(isLabel) "margin-top: 20px;" else "margin-top: 10px;",
        if(isLabel) tags$p(tags$strong(
            option
        )) else textInput(
            ns(paste('input', option, sep = "_")), 
            label = option, 
            value = options[[option]]
        )
    )
}
getOptionFamilyTags <- function(optionFamily, optionFamilies){
    options <- optionFamilies[[optionFamily]]
    border <- if(optionFamily != rev(names(optionFamilies))[1]) "border-bottom: 1px solid #ddd;" else ""
    fluidRow(
        style = paste("padding: 0 0 10px 0;", border),
        lapply(seq_along(options), function(i){
            tagList(
                if(i %% 3 == 1) getOptionTag(if(i == 1) optionFamily else "") else "",
                getOptionTag(names(options)[i], options)
            )
        })
    )
}
output$optionFamilies <- renderUI({
    config <- pipelineConfig()
    req(config)
    req(config$actions)
    tabActions <- if(length(config$actions) > 1) input$actions else config$actions
    tabs <- lapply(tabActions, function(action){
        optionFamilies <- config$optionFamilies[[action]]
        tabPanel(
            action, 
            tags$div(
                style = "padding-left: 15px;",
                lapply(names(optionFamilies), getOptionFamilyTags, optionFamilies)
            )
        )
    })
    tabs$id <- "pipelineRunnerOptionTabs"
    tabs$width <- 12
    do.call(tabBox, tabs)
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
