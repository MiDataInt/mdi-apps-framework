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

#----------------------------------------------------------------------
# add tooltips and documentation
#----------------------------------------------------------------------
addMdiTooltips(
    session, 
    list(
        c("setName", "Give this configuration set a short, useful name")
    )
)
addPRDocs('docs', "docs/server-deployment/pipeline-runner", "create-and-edit-job-configuration-files")
#----------------------------------------------------------------------
# initialize step settings
#----------------------------------------------------------------------
settings <- settingsServer(
    'settings', 
    id, 
    resettable = FALSE 
)
#----------------------------------------------------------------------
# initialize job file creation
#----------------------------------------------------------------------
sourceFileInput <- sourceFileInputServer(
    'fileInput', 
    appName = 'pipelineRunner', 
    createButtonServer = createJobFileServer
)
#----------------------------------------------------------------------
# initialize the list of related job files (parent table)
#----------------------------------------------------------------------
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
    id = 'jobFiles',
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

#----------------------------------------------------------------------
# handle an incoming <data>.yml, or a cold start from the launch page
#----------------------------------------------------------------------
loadSourceFile <- function(incomingFile, ...){
    stopSpinner(session, 'loadSourceFile')
    req(incomingFile)
    req(incomingFile$path) 
    req(file.exists(incomingFile$path))
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
    # selectRows(jobFiles$proxy, length(jobFiles$list)) # auto-select the new (last) job file row
}
# add an _additional_ job file uploaded by user via step 1 (not via the launch page)
handleExtraFile <- function(reactive){
    x <- reactive()
    req(x)
    loadSourceFile(x)
}
observeEvent(sourceFileInput$file(), {
    handleExtraFile(sourceFileInput$file)
})
observeEvent(sourceFileInput$createButtonData(), {
    handleExtraFile(sourceFileInput$createButtonData)
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
observeEvent(jobFiles$selected(), {
    selectedRow <- jobFiles$selected()
    isSelection <- !is.null(selectedRow) && !is.na(selectedRow)
    shinyjs::toggle(
        selector = "span.requiresJobFile", 
        condition = isSelection
    )
    shinyjs::toggle(
        selector = "div.requiresJobFileMessage", 
        condition = !isSelection
    )
    html(
        id = session$ns("jobFiles-titleSuffix"), 
        asis = TRUE, 
        html = if(is.na(selectedRow)) "" else paste0(" - ", jobFiles$list[[selectedRow]]$name)
    )
})

#----------------------------------------------------------------------
# set the job configuration edit mode
#----------------------------------------------------------------------
editModes <- list(none = "none", inputs = "inputs", editor = "editor")
editMode <- reactiveVal(editModes$none)
observeEvent({
    settings$Job_Files()
    activeJobFile()
}, {
    oldMode <- editMode()
    req(oldMode) 
    isInputs <- settings$Job_Files()$Job_File_Edit_Mode$value == "User Inputs"
    newMode <- if(isInputs) editModes$inputs else editModes$editor
    if(oldMode == newMode) return(NULL)
    if(isPendingChanges()){
        showUserDialog(
            "Changes are Pending", 
            tags$p("You have pending changes in one or more job files."), 
            tags$p("Please save or discard all pending changes before switching editor modes."), 
            size = "s", 
            type = 'okOnly'
        )
        settings$set("Job_Files", "Job_File_Edit_Mode", if(isInputs) "Script Editor" else "User Inputs")
        return(NULL)
    }
    shinyjs::toggle(id = "uiBasedJobEditing",   condition =  isInputs)
    shinyjs::toggle(id = "textBasedJobEditing", condition = !isInputs)
    editMode(
        if(is.null(activeJobFile())) editModes$none 
        else if(isInputs) editModes$inputs 
        else editModes$editor
    )
})

#----------------------------------------------------------------------
# initialize the job file editors
#----------------------------------------------------------------------
editors <- list(
    editor = jobFileTextEditorServer( "textEditor",  editMode, activeJobFile),
    inputs = jobFileInputEditorServer("inputEditor", editMode, activeJobFile)
)
isPendingChanges <- function(path = NULL){
    editMode <- editMode()
    if(editMode == editModes$none) return(FALSE)
    if(is.null(path)) path <- names(jobFiles$list)
    for(x in path) if(editors[[editMode]]$pending(x)) return(TRUE)
    FALSE
}

#----------------------------------------------------------------------
# job file saving UI elements (buttons and links)
#----------------------------------------------------------------------
# main save button
saveJobFileId <- "saveJobFile"
output$saveJobFileUI <- renderUI({ # dynamically colored button for job file saving
    jobFile <- activeJobFile()
    disabled <- !isPendingChanges(jobFile$path)
    style <- if(disabled) "default" else "success"
    bsButton(session$ns(saveJobFileId), "Save Job Config", style = style, disabled = disabled)
})

# save as link
saveJobFileAsId <- "saveJobFileAs"
output$saveJobFileAsUI <- renderUI({ # dynamically colored button for job file saving
    jobFile <- activeJobFile()
    serverSaveFileLinkUI(session$ns(saveJobFileAsId), "Save As...", jobFile$pipeline, ".yml")
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

# *** job file save action ***
writeError <- reactiveVal(NULL)
observeEvent(writeError(), {
    showUserDialog(
        title = "Job File Error",
        lapply(writeError()$message, tags$p),
        type = "okOnly"
    )
}, ignoreInit = TRUE)
observeEvent(input[[saveJobFileId]], {
    editMode <- editMode()
    if(editMode == editModes$none) return(FALSE)
    path <- activeJobFile()$path
    showUserDialog(
        "Save Configuration Changes", 
        tags$p(paste(
            "Save changes to configuration file?"
        )), 
        tags$p(path, style = "margin-left: 2em;"),
        callback = function(...) {
            isolate({
                write <- editors[[editMode]]$write(path, path) 
                if(write$success) editors[[editMode]]$save(path)
                else writeError(list(
                    message = write$message, 
                    force = sample(1e8, 1)
                ))
            })
        }, 
        type = 'saveCancel',
        size = 'm'
    )
})

# *** job file save as action ***
saveJobFileAs <- function(newPath){
    editMode <- editMode()
    if(editMode == editModes$none) return(FALSE)
    oldPath <- activeJobFile()$path
    editors[[editMode]]$write(newPath, oldPath)
    loadSourceFile(list(path = newPath))
    editors[[editMode]]$saveAs(newPath, oldPath)
}

# *** job file revert action, i.e., discard changes ***
observeEvent(input$discardChanges, {
    editMode <- editMode()
    if(editMode == editModes$none) return(FALSE)
    path <- activeJobFile()$path
    req(isPendingChanges(path))
    showUserDialog(
        "Confirm Discard Changes", 
        tags$p(paste(
            "Discard any changes you have made to the following job configuration file?",
            "The file will be reverted to its previously saved state.",
            "This cannot be undone."
        )), 
        tags$p(path, style = "margin-left: 2em;"),
        callback = function(...) {
            editors[[editMode]]$discard(path)
        }, 
        type = 'discardCancel',
        size = 'm'
    )
})

#----------------------------------------------------------------------
# define bookmarking actions
#----------------------------------------------------------------------
observe({
    bm <- getModuleBookmark(id, module, bookmark, locks)
    req(bm)
    settings$replace(bm$settings)
    updateTextInput(session, 'analysisSetName', value = bm$outcomes$analysisSetName)
    jobFiles$list <- bm$outcomes$jobFiles
})

#----------------------------------------------------------------------
# set return values as reactives that will be assigned to app$data[[stepName]]
#----------------------------------------------------------------------
list(
    input = input,
    settings = settings$all_,
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
