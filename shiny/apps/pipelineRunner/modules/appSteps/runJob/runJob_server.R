#----------------------------------------------------------------------
# reactive components to launch and monitor pipeline jobs
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
runJobServer <- function(id, options, bookmark, locks) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        module <- 'runJob' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize module
#----------------------------------------------------------------------
currentJobFilePath <- reactiveVal(NULL)
jobFiles <- selectJobFilesServer(
    id = 'jobFiles',
    parentId = id,
    parentOptions = options
)

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
# cascade to show the status of a selected job configuration file
#----------------------------------------------------------------------
nullStatusTable <- data.table(
    jobName = "no submitted jobs",
    jobID = "",
    array = "",
    start_time = "",
    exit_status = "",
    walltime = "",
    maxvmem = ""
)
statusTable <- reactive({
    input$refreshStatus
    jobFile <- activeJobFile()
    req(jobFile)

    # write the status to file using mdi status (but don't use it's formatted return value)
    startSpinner(session, 'mdi status')
    args <- c('status', jobFile$path)
    x <- runMdiCommand(args, collapse = FALSE)
    req(x$success) 

    # recover the tab-delimited status text from disk
    dataDir <- paste0(".", jobFile$name, ".data")
    statusFile <- paste0(jobFile$name, ".status")
    statusFile <- file.path(jobFile$directory, dataDir, statusFile)
    if(!file.exists(statusFile)) {
        stopSpinner(session, 'mdi status')
        return(nullStatusTable)
    }

    # parse the status file into a tab delimited table
    x <- strsplit(slurpFile(statusFile), "\n")[[1]]
    i1 <- which(startsWith(x, "jobName"))
    x <- paste(x[i1:length(x)], collapse = "\n")
    x <- fread(text = x)
    stopSpinner(session, 'mdi status')
    x[, .(
        jobName,
        jobID,
        array,
        start_time,
        exit_status,
        walltime,
        maxvmem
    )]    
})
output$statusTable <- renderDT(
    { statusTable() },
    options = list(
        paging = FALSE,      
        searching = FALSE            
    ),
    class = "display table-compact-4",
    escape = TRUE, 
    selection = 'single', 
    editable = FALSE, 
    rownames = FALSE # must be true for editing to work, not sure why (datatables peculiarity)
)

#----------------------------------------------------------------------
# generic handlers for MDI job-manager commands
#----------------------------------------------------------------------
runJobManagerCommand <- function(command, jobId = NULL, dryRun = TRUE, force = FALSE,
                                 errorString = 'mdi error:'){
    setOutputData(NULL, NULL, FALSE)
    jobFile <- activeJobFile()
    req(jobFile)
    startSpinner(session, command)
    jobId  <- if(is.null(jobId)) ""  else c("--job", jobId)
    dryRun <- if(dryRun) "--dry-run" else ""
    force  <- if(force)  "--force"   else ""
    args <- c(command, jobId, dryRun, force, jobFile$path)
    data <- runMdiCommand(args)
    if(!data$success) return( setOutputData(command, data) )
    data$success <- isMdiSuccess(data$results)
    setOutputData(command, data)
}

#----------------------------------------------------------------------
# cascade to show the log report of a selected job
#----------------------------------------------------------------------
selectedJob <- rowSelectionObserver('statusTable', input)
fillOutput_report <- function(){
    setOutputData(NULL, NULL, FALSE)
    jobFile <- activeJobFile()
    req(jobFile)
    statusTable <- statusTable()
    req(statusTable)
    rowI <- selectedJob()
    req(rowI)
    jobId <- statusTable[rowI, jobID]
    req(jobId)
    runJobManagerCommand('report', jobId = jobId, dryRun = FALSE)
}
observeEvent(selectedJob(), {
    fillOutput_report()
})

#----------------------------------------------------------------------
# inspect: display the complete set of job configuration options
# handles by direct calls to launcher.pl
#----------------------------------------------------------------------
observeEvent(input$inspect, {
    setOutputData(NULL, NULL, FALSE)
    jobFile <- activeJobFile()
    req(jobFile)
    command <- 'inspect'
    startSpinner(session, command)
    args <- c(jobFile$pipeline, jobFile$path, '--dry-run')
    data <- runMdiCommand(args)
    if(!data$success) return( setOutputData(command, data) )
    data$success <- isMdiSuccess(data$results)
    setOutputData(command, data)
})

#----------------------------------------------------------------------
# dry run and live submit of jobs
#----------------------------------------------------------------------
observeEvent(input$submit, {
    runJobManagerCommand('submit')
})
observeEvent(input$extend, {
    runJobManagerCommand('extend')
})

#----------------------------------------------------------------------
# job configuration file status history rollback and purging
#----------------------------------------------------------------------
# observeEvent(input$rollback, {
#     runJobManagerCommand('rollback', force = TRUE)
# })
# observeEvent(input$purge, {
#     runJobManagerCommand('purge', force = TRUE)
# })

#----------------------------------------------------------------------
# render all command outputs
#----------------------------------------------------------------------
nullOutput <- list(type = "", text = NULL)
outputData <- reactiveVal(nullOutput)
setOutputData <- function(command, data, stopSpinner = TRUE){
    if(stopSpinner) stopSpinner(session, command)          
    outputData(list(
        command = if(is.null(data)) "" else command,
        data = data
    ))
}
output$output <- renderText({
    x <- outputData()$data
    req(x)
    toggle('refreshOutput', condition = !is.null(getFillFn()))    
    toggleClass(
        selector = ".command-output-wrapper pre", 
        class = "command-output-error", 
        condition = !x$success
    )
    x$results
})

#----------------------------------------------------------------------
# enable output refresh link
#----------------------------------------------------------------------
getFillFn <- function(){
    req(outputData()$command)
    fn <- paste('fillOutput', outputData()$command, sep = "_")
    if(exists(fn)) get(fn) else NULL
}
observeEvent(input$refreshOutput, {
    fn <- getFillFn()
    req(fn)
    fn()
})

#----------------------------------------------------------------------
# define bookmarking actions
#----------------------------------------------------------------------
observe({
    bm <- getModuleBookmark(id, module, bookmark, locks)
    req(bm)
    
})

#----------------------------------------------------------------------
# set return value
#----------------------------------------------------------------------
list(
    output = list(
    )
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
