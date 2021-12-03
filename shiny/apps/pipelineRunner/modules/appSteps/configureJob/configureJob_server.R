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
# initialize module
#----------------------------------------------------------------------
pipelineSuiteDirs <-  getPipelineSuiteDirs()
pipelineDirs <- getPipelineDirs(pipelineSuiteDirs)
pipelineSuites <- getInstalledPipelineSuites(pipelineDirs)
serverServerFileButtonServer("saveJobFile", input, session, "yml", saveFn = function(file) NULL)

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
output$optionFamilies <- renderUI({
    config <- pipelineConfig()
    req(config)
    req(config$actions)
    tabActions <- if(length(config$actions) > 1) input$actions else config$actions
    tabs <- lapply(tabActions, function(action){
        optionFamilies <- config$optionFamilies[[action]]
        tabPanel(
            action, 
            lapply(names(optionFamilies), function(optionFamily){
                options <- optionFamilies[[optionFamily]]
                tagList(
                    fluidRow(tags$p(strong(optionFamily))),
                    fluidRow(
                        lapply(names(options), function(option){
                            column(
                                width = 3,
                                textInput(ns(paste('input', option, sep = "_")), 
                                          label = option, value = options[[option]])
                            )
                        })   
                    ),
                    tags$hr()
                )
            })
        )
    })
    tabs$id <- "pipelineRunnerOptionTabs"
    tabs$width <- 12
    do.call(tabBox, tabs)
})

#----------------------------------------------------------------------
# handle an incoming job.yml, or a cold start from the launch page #launchPipelineRunner link
#----------------------------------------------------------------------
loadSourceFile <- function(incomingFile, suppressUnlink = FALSE){
    reportProgress('launch', module)


    startSpinner(session, 'launch pipelineRunner')

    stopSpinner(session, 'launch pipelineRunner')

}

#----------------------------------------------------------------------
# populate the top level inputs and outputs for suite, pipeline and job file
#----------------------------------------------------------------------
output$jobFilePath <- renderText({
    "pending"
})

#----------------------------------------------------------------------
# define bookmarking actions
#----------------------------------------------------------------------
observe({
    bm <- getModuleBookmark(id, module, bookmark, locks)
    req(bm)
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
      
    ),
    loadSourceFile = loadSourceFile
    # ,
    # isReady = reactive({TRUE}) #reactive({ getStepReadiness(options$source, samples$list) })
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
