#----------------------------------------------------------------------
# reactive components for cold creation of a new Stage 1 job configuration file
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
createJobFileServer <- function(id, parentId){
    moduleServer(id, function(input, output, session){
        module <- 'createJobFile' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize installed pipelines
#----------------------------------------------------------------------
pipelineSuiteDirs <- getPipelineSuiteDirs()
pipelineDirs      <- getPipelineDirs(pipelineSuiteDirs)
pipelineSuites    <- getInstalledPipelineSuites(pipelineDirs)

#----------------------------------------------------------------------
# prepare the button dialog
#----------------------------------------------------------------------
lastCreated <- list() # buffer for remembering the state of the create dialog
createDirId <- 'directory'
getCreateDirId <- "getDirectory"
createDirLabel <- HTML(paste('Server Directory', {
    serverChooseDirIconServer(getCreateDirId, input, session, chooseFn = function(dir) {
        updateTextInput(session, createDirId, value = dir$dir)
    })
    serverChooseDirIconUI(session$ns(getCreateDirId), class = "mdi-dir-icon")
}))

#----------------------------------------------------------------------
# show the button dialog
#----------------------------------------------------------------------
observeEvent(input$button, {
    activeSuite <- if(is.null(lastCreated$suite)) pipelineSuites[1] else lastCreated$suite
    showUserDialog(
        "Create a New Job File", 
        fluidRow(
            column(width = 6,
                selectInput(session$ns('suite'), 'Tool Suite', choices = pipelineSuites, 
                            selected = activeSuite, width = "100%")
            ),
            column(width = 6,
                selectInput(session$ns('pipeline'), 'Pipeline', choices = getInstalledPipelines(activeSuite), 
                            selected = lastCreated$pipeline, width = "100%")
            )      
        ),
        textInput(session$ns(createDirId), createDirLabel, value = lastCreated$directory, width = "100%"),
        fluidRow(
            style = "margin-top: 1em;",
            column(width = 6,
                textInput(session$ns('file'), 'File Name', value = lastCreated$file)
            ),
            column(width = 2, ".yml", style = "margin-top: 2em; padding-left: 0;"),
            column(width = 4, style = "margin-top: 1.25em;", 
                   checkboxInput(session$ns("allOptions"), "Include All Options", value = lastCreated$allOptions))  
        ),
        size = "m",
        type = 'custom',
        footer = tagList( 
            modalButton("Cancel"),
            bsButton(session$ns("save"), "Save Job File", style = "success")
        )
    )
})
suiteName <- reactive({ # suiteName() is just the name of the suite, e.g., mdi-johndoe-apps
    req(input$suite)
    basename(input$suite)
})
observeEvent(input$suite, {
    req(input$suite)
    updateSelectInput(session, 'pipeline', choices = getInstalledPipelines(input$suite))
})

#----------------------------------------------------------------------
# monitor and validate the state of the inputs
#----------------------------------------------------------------------
buttonStates <- list(
    dirNotFound = list(
        label = "Directory Not Found",
        style = "danger"
    ),
    ready = list(
        label = "Save Job File",
        style = "success"
    ),
    overwrite = list(
        label = "Overwrite Job File",
        style = "warning"   
    )
)
observeEvent({
    input$directory
    input$file
}, {
    dirNotFound <- !is.null(input$directory) && input$directory != "" && !dir.exists(input$directory)
    disabled <- is.null(input$directory) || input$directory == "" || !dir.exists(input$directory) || 
                is.null(input$file)      || input$file == ""
    state <- if(dirNotFound){
        buttonStates$dirNotFound
    } else if(disabled){
        buttonStates$ready
    } else {
        if(file.exists(jobFilePath())) buttonStates$overwrite else buttonStates$ready
    }
    updateButton(session, session$ns("save"), 
                 label = state$label, style = state$style, disabled = disabled)
})

#----------------------------------------------------------------------
# react to Save button by creating a new job configuration file
#----------------------------------------------------------------------
jobFilePath <- reactive({
    req(input$directory)
    req(input$file)
    file <- if(endsWith(input$file, ".yml")) input$file else paste0(input$file, ".yml")
    file.path(input$directory, file)
})
newFile <- reactiveVal(list())
observeEvent(input$save, {
    removeModal()    
    startSpinner(session, "save job file")
    lastCreated <<- list(
        suite = input$suite,
        pipeline = input$pipeline,
        directory = input$directory,
        file = input$file,
        allOptions = input$allOptions
    )     
    jobFilePath <- jobFilePath()
    defaults <- getJobEnvironmentDefaults(dirname(jobFilePath), input$pipeline)
    writeDataYml(jobFilePath, suiteName(), input$pipeline, defaults, allOptions = input$allOptions)
    stopSpinner(session, "save job file")
    newFile(list(path = jobFilePath))
})

#----------------------------------------------------------------------
# return value
#----------------------------------------------------------------------
newFile

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
