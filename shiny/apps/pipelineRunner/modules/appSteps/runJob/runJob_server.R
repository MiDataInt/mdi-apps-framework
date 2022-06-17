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
if(serverEnv$SUPPRESS_PIPELINE_RUNNER) return(NULL)

#----------------------------------------------------------------------
# initialize module
#----------------------------------------------------------------------
currentJobFilePath <- reactiveVal(NULL)
jobFiles <- selectJobFilesServer(
    id = 'jobFiles',
    parentId = id,
    parentOptions = options
)
addPRDocs('docs', "docs/server-deployment/pipeline-runner", "execute-and-monitor-jobs")

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
invalidateStatusTable <- reactiveVal(0)
statusTable <- reactive({
    input$refreshStatus
    invalidateStatusTable()
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
    x <- x[, .(
        jobName,
        jobID,
        array,
        start_time,
        exit_status,
        walltime,
        maxvmem
    )]   
    y <- data.table(
        delete = tableActionLinks(ns(deleteLinkId), nrow(x), 'Delete', 
                                  allow = !is.numeric(x$exit_status))  
    )
    cbind(y, x)
})
output$statusTable <- renderDT(
    { statusTable() },
    options = list(
        paging = FALSE,      
        searching = FALSE            
    ),
    class = "display table-compact-4",
    escape = FALSE, 
    selection = 'single', 
    editable = FALSE, 
    rownames = FALSE # must be true for editing to work, not sure why (datatables peculiarity)
)

#----------------------------------------------------------------------
# enable job deletetion from within the status table
#----------------------------------------------------------------------
deleteLinkId <- 'deleteLink'
observeEvent(input[[deleteLinkId]], {
    statusTable <- statusTable()
    req(statusTable)
    row <- getTableActionLinkRow(input, deleteLinkId)
    jobName <- statusTable[row, jobName]
    jobId   <- statusTable[row, jobID]
    showUserDialog(
        "Confirm Job Deletion", 
        tags$p("Delete / kill / cancel this job from the cluster job queue?"),
        tags$p(jobName), 
        tags$p(jobId),
        callback = function(parentInput) {
            # runJobManagerCommand('delete', jobId = jobId, dryRun = FALSE, force = TRUE)
            invalidateStatusTable( invalidateStatusTable() + 1 )
        },
        size = "s", 
        type = 'deleteCancel'
    )
})

#----------------------------------------------------------------------
# generic handler for MDI job-manager commands
#----------------------------------------------------------------------
runJobManagerCommand <- function(command, jobId = NULL, dryRun = TRUE, force = FALSE,
                                 errorString = 'mdi error:'){
    setOutputData(NULL, NULL, NULL, FALSE)
    jobFile <- activeJobFile()
    req(jobFile)
    startSpinner(session, command)
    jobId  <- if(is.null(jobId)) ""  else c("--job", jobId)
    dryRun <- if(dryRun) "--dry-run" else ""
    force  <- if(force)  "--force"   else ""
    args <- c(command, jobId, dryRun, force, jobFile$path)
    data <- runMdiCommand(args)
    if(!data$success) return( setOutputData(command, args, data) )
    data$success <- isMdiSuccess(data$results)
    setOutputData(command, args, data)
}

#----------------------------------------------------------------------
# cascade to show the log report of a selected job
#----------------------------------------------------------------------
selectedJob <- rowSelectionObserver('statusTable', input)
fillOutput_report <- function(){
    setOutputData(NULL, NULL, NULL, FALSE)
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
# handled by direct call to launcher.pl
#----------------------------------------------------------------------
observeEvent(input$inspect, {
    setOutputData(NULL, NULL, NULL, FALSE)
    jobFile <- activeJobFile()
    req(jobFile)
    command <- 'inspect'
    startSpinner(session, command)
    args <- c(jobFile$pipeline, jobFile$path, '--dry-run')
    data <- runMdiCommand(args)
    if(!data$success) return( setOutputData(command, args, data) )
    data$success <- isMdiSuccess(data$results)
    setOutputData(command, args, data)
})

#----------------------------------------------------------------------
# dry run job submission buttons
#----------------------------------------------------------------------
observeEvent(input$submit, {
    runJobManagerCommand('submit', force = TRUE)
})
observeEvent(input$extend, {
    runJobManagerCommand('extend', force = TRUE)
})

#----------------------------------------------------------------------
# status history rollback and purging
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
setOutputData <- function(command, args, data, stopSpinner = TRUE){
    if(stopSpinner) stopSpinner(session, command)          
    outputData(list(
        command = if(is.null(data)) "" else command,
        args = args,
        data = data
    ))
}
output$command <- renderText({ # show a simplified version of the command output being displayed
    x <- outputData()$args
    req(x)  
    paste(sapply(c("mdi", x), basename), collapse = " ")
})
output$output <- renderText({ # the output returned by 'mdi <command>'
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
# enable final execution, i.e., after (from within) a --dry-run display
#----------------------------------------------------------------------
executeButtonMetadata <- list(
    submit   = c("Submit",   "success"),
    extend   = c("Extend",   "success"),
    rollback = c("Rollback", "danger"),
    purge    = c("Purge",    "danger")
)
output$executeButton <- renderUI({ # render the Execute button
    x <- outputData()$command
    req(x)  
    if(x == "report") return(downloadPackageButton())
    d <- executeButtonMetadata[[x]]
    req(d)
    label <- paste('Execute', d[1])
    bsButton(ns('execute'), label, style = d[2], width = "100%")
})
observeEvent(input$execute, { # act on the execute button click
    command <- outputData()$command
    req(command)
    if(command == "report") return()
    args <- outputData()$args
    req(args)
    command <- 'execute'
    startSpinner(session, command)
    args <- args[args != "--dry-run"]
    data <- runMdiCommand(args)
    if(!data$success) return( setOutputData(command, args, data) )
    data$success <- isMdiSuccess(data$results)
    if(!data$success) return( setOutputData(command, args, data) )
    setOutputData(NULL, NULL, NULL)
    invalidateStatusTable( invalidateStatusTable() + 1 )
})

#----------------------------------------------------------------------
# download a data package from a job for use in Stage 2 apps
#----------------------------------------------------------------------
packageFile <- reactiveVal(NULL)
downloadPackageButton <- function(){
    packageFile(NULL)
    x <- outputData()$data
    req(x)
    x <- x$results
    req(x)
    x <- strsplit(x, "\n")[[1]]
    i <- max(which(grepl("writing Stage 2 package file", x)))
    req(i)
    i <- i + 1 # this line carries the name of the most recently written data package.zip
    packageFile(x[i])
    downloadButton(ns('download'), label = "Download Package", 
                   icon = NULL, width = "100%", # style mimics bsButton style=primary
                   style = "color: white; background-color: #3c8dbc; width: 100%; border-radius: 3px;")
}
output$download <- downloadHandler(
    filename = function(){
        basename(packageFile())
    },
    content = function(tmpFile){
        file.copy(packageFile(), tmpFile)
    }
)

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
