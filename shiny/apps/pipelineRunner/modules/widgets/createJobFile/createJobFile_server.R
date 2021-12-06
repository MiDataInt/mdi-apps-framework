#----------------------------------------------------------------------
# reactive components for cold creation of a new Stage 1 job configuration file
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
createJobFileServer <- function(id, parentId){
    moduleServer(id, function(input, output, session){
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        parentNS <- function(id_) paste(parentId, id, id_, sep = "-")
        module <- 'createJobFile' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize installed pipelines
#----------------------------------------------------------------------
pipelineSuiteDirs <- getPipelineSuiteDirs()
pipelineDirs      <- getPipelineDirs(pipelineSuiteDirs)
pipelineSuites    <- getInstalledPipelineSuites(pipelineDirs)
newFile <- reactiveVal(NULL)

#----------------------------------------------------------------------
# cascade update to suite and pipeline selectInputs
#----------------------------------------------------------------------
isolate({ # input$suite is the full path of the suite directory
    updateSelectInput('suite', choices = pipelineSuites, session = session)
})
suiteName <- reactive({ # suiteName() is just the name of the suite, e.g., mdi-johndoe-apps
    req(input$suite)
    rev(strsplit(input$suite, '/')[[1]])[1]
})
observeEvent(input$suite, {
    req(input$suite)
    updateSelectInput('pipeline', choices = getInstalledPipelines(input$suite), session = session)
})

#----------------------------------------------------------------------
# cascade update to create new file button
#----------------------------------------------------------------------
output$createJobFileUI <- renderUI({ # dynamically colored button for job file saving
    req(input$pipeline)
    serverSaveFileButtonUI(parentNS("createJobFile"), "Create New", input$pipeline, ".yml",
                           buttonType = "default")
})
serverSaveFileButtonServer("createJobFile", input, session, "yml", 
                           default_type = 'job_default', saveFn = createJobFile)
createJobFile <- function(jobFilePath){
    args <- c(input$pipeline, 'template') 
    template <- runMdiCommand(args)
    req(template$success)
    cat(template$results, file = jobFilePath)
    newFile(list(path = jobFilePath))
}

#----------------------------------------------------------------------
# return value
#----------------------------------------------------------------------
list(
    pipelineSuiteDirs = pipelineSuiteDirs, # all are static objects
    pipelineDirs = pipelineDirs,
    pipelineSuites = pipelineSuites,
    file = newFile # reactive to signal a newly created file to add to a parent list
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
