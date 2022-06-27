#----------------------------------------------------------------------
# reactive components to launch and monitor pipeline jobs
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
runJobServer <- function(id, options, bookmark, locks) {
    moduleServer(id, function(input, output, session) {
        module <- 'runJob' # for reportProgress tracing
#----------------------------------------------------------------------
if(serverEnv$SUPPRESS_PIPELINE_RUNNER) return(NULL)

#----------------------------------------------------------------------
# initialize module
#----------------------------------------------------------------------
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
# generic handler for MDI job-manager commands
#----------------------------------------------------------------------
runJobManagerCommand <- function(command, jobId = NULL, 
                                 dryRun = TRUE, force = FALSE,
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
    setOutputData(command, args, data)
}

#----------------------------------------------------------------------
# top-level action buttons for a selected job configuration file
#----------------------------------------------------------------------
# buttons to get ready to submit
observeEvent(input$inspect, {
    runJobManagerCommand('inspect', dryRun = FALSE) # inspect itself enforces --dry-run
})
observeEvent(input$mkdir, {
    runJobManagerCommand('mkdir', force = TRUE)
})
# dry-run job submission buttons (result panels reveal buttons for live execution)
observeEvent(input$submit, {
    runJobManagerCommand('submit', force = TRUE)
})
observeEvent(input$extend, {
    runJobManagerCommand('extend', force = TRUE)
})
# status history rollback and purging
observeEvent(input$rollback, {
    runJobManagerCommand('rollback', force = TRUE) 
})
observeEvent(input$purge, {
    runJobManagerCommand('purge', force = TRUE) 
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
    x[, array := sapply(array, function(d) if(is.na(d) || d == "") NA else {
        paste(range(as.integer(strsplit(d, ",")[[1]])), collapse = "-")
    })]
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
        delete = tableActionLinks(
            session$ns(deleteLinkId), 
            nrow(x), 
            'Delete', 
            allow = !check.numeric(x$exit_status)
        )  
    )
    stopSpinner(session, 'mdi status')    
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
# enable job deletion from within the status table
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
        tags$p(
            tags$strong("Job Name: "), 
            jobName,
            style = "margin-left: 0.5em;"
        ),
        tags$p(
            tags$strong("Job ID: "), 
            jobId,
            style = "margin-left: 0.5em;"
        ),
        callback = function(parentInput) {
            runJobManagerCommand('delete', jobId = jobId, dryRun = FALSE, force = TRUE)
            invalidateStatusTable( invalidateStatusTable() + 1 )
        },
        size = "s", 
        type = 'deleteCancel'
    )
})

#----------------------------------------------------------------------
# cascade to show the initial, complete log report of a selected job
#----------------------------------------------------------------------
selectedJobI <- rowSelectionObserver('statusTable', input)
selectedJob <- reactive({
    statusTable <- statusTable()
    req(statusTable)
    rowI <- selectedJobI()
    req(rowI)
    statusTable[rowI, ]
})
selectedJobId <- reactive({
    job <- selectedJob()
    req(job)
    job[, jobID]
})
observeEvent(selectedJobI(), { 
    setOutputData(NULL, NULL, NULL, FALSE)
    jobId <- selectedJobId()
    req(jobId)
    runJobManagerCommand('report', jobId = jobId, dryRun = FALSE)        
})

#----------------------------------------------------------------------
# inputs panel to enable enhanced, task-level reporting
#----------------------------------------------------------------------
allTasks <- "all tasks"
taskSelector <- reactive({
    tasks <- selectedJob()[, array]
    if(is.na(tasks)) "" else tagList(
        column(
            width = 1,
            style = "padding-right: 0;",
            tags$strong("Task #", style = "float: right; margin-top: 6px;")
        ), 
        column(
            width = 2,
            selectInput(
                session$ns("taskNumber"), 
                NULL, 
                choices = c(
                    allTasks, 
                    as.character(1:max(as.integer(strsplit(tasks, "-")[[1]])))
                )
            )
        )
    )
})
reportButton <- function(){
    column(
        width = 2,
        bsButton(session$ns("report"), "Log Report", style = "default", width = "100%")
    )      
}
enableTaskLevelButtons <- reactive({
    tasks <- selectedJob()[, array]
    if(is.na(tasks)) return(TRUE) # non-array, single-task job
    req(input$taskNumber)
    req(input$taskNumber != allTasks)    
})
output$ls_ <- renderUI({
    req(enableTaskLevelButtons())
    column(
        width = 2,
        bsButton(session$ns("ls"), "List Output Files", style = "default", width = "100%")
    )
})
output$top_ <- renderUI({
    req(enableTaskLevelButtons())
    req(!check.numeric(selectedJob()[, exit_status]))
    column(
        width = 2,
        bsButton(session$ns("top"), "Process Metrics", style = "default", width = "100%")
    )
})
output$taskOptions <- renderUI({
    job <- selectedJob()
    req(job)
    fluidRow(
        style = "margin-top: 0.5em;",
        box(
            width = 12,
            status = 'primary',
            solidHeader = FALSE,
            style = "padding: 10px 0 10px 15px;",
            fluidRow(
                taskSelector(),
                reportButton(),
                uiOutput(session$ns('ls_')),
                uiOutput(session$ns('top_'))
            )
        )
    )        
})

# ----------------------------------------------------------------------
# task-level reporting/monitoring actions
# ----------------------------------------------------------------------
runTaskLevelCommand <- function(command){
    jobId <- selectedJobId()
    req(jobId)
    tasks <- selectedJob()[, array]
    taskNumber <- if(is.na(tasks)) NA else input$taskNumber
    jobId <- paste0(jobId, if(is.null(taskNumber) || is.na(taskNumber) || taskNumber == allTasks){
        ""
    } else {
        paste0("[", taskNumber, "]")
    })
    runJobManagerCommand(command, jobId = jobId, dryRun = FALSE) 
}
observeEvent({ # update command output as selected task changes
    selectedJobId()
    input$taskNumber
}, {
    req(input$taskNumber)
    isAllTasks <- input$taskNumber == allTasks
    command <- outputData()$command
    req(command)  
    command <- if(isAllTasks) "report" else command
    runTaskLevelCommand(command)
})
observeEvent(input$report, { runTaskLevelCommand('report') }) # respond to task button clicks
observeEvent(input$ls,     { runTaskLevelCommand('ls') })
observeEvent(input$top,    { runTaskLevelCommand('top') })

#----------------------------------------------------------------------
# render all command outputs
#----------------------------------------------------------------------
outputData <- reactiveVal(NULL)
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
    toggleClass(
        selector = ".command-output-wrapper pre", 
        class = "command-output-error", 
        condition = !x$success
    )
    paste(x$results, collapse = "\n")
})

#----------------------------------------------------------------------
# enable output refresh icon link
#----------------------------------------------------------------------
observeEvent(input$refreshOutput, {
    x <- outputData()
    req(x)  
    startSpinner(session, x$command)
    data <- runMdiCommand(x$args)
    setOutputData(x$command, x$args, data)
})

#----------------------------------------------------------------------
# enable final execution, i.e., after (from within) a --dry-run display
#----------------------------------------------------------------------
executeButtonMetadata <- list(
    mkdir    = c("Make Directory", "primary", suppressIf = "all output directories already exist"),
    submit   = c("Submit",         "success"),
    extend   = c("Extend",         "success"),
    rollback = c("Rollback",       "danger"),
    purge    = c("Purge",          "danger")
)
output$executeButton <- renderUI({ # render the Execute ... button
    x <- outputData()$command
    req(x)  
    if(x == "report") return(downloadPackageButton())
    d <- executeButtonMetadata[[x]]
    req(d)
    if(!is.null(d[3]) && !is.na(d[3])){ # handle conditional button display
        data <- outputData()$data
        req(data)
        results <- data$results
        req(results)
        if(any(grepl(d[3], results))) return(NULL)
    }
    label <- paste('Execute', d[1]) # all buttons labeled "Execute <Command>"
    bsButton(session$ns('execute'), label, style = d[2], width = "100%")
})
observeEvent(input$execute, { # act on the Execute button click
    command <- outputData()$command
    req(command)
    if(command == "report") return()
    args <- outputData()$args
    req(args)
    command <- 'execute'
    startSpinner(session, command)
    args <- args[args != "--dry-run"]
    data <- runMdiCommand(args)
    setOutputData(command, args, data)
    if(data$success) invalidateStatusTable( invalidateStatusTable() + 1 )
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
    is <- which(grepl("writing Stage 2 package file", x))
    req(is)
    i <- max(is)
    req(i)
    i <- i + 1 # this line carries the name of the most recently written data package.zip
    packageFile(x[i])
    downloadButton(session$ns('download'), label = "Download Package", 
                   icon = NULL, width = "100%", # style mimics bsButton style=primary
                   style = "color: white; background-color: #3c8dbc; width: 100%; border-radius: 3px; float: right;")
}
output$download <- downloadHandler(
    filename = function() basename(packageFile()),
    content  = function(tmpFile) file.copy(packageFile(), tmpFile)
)

#----------------------------------------------------------------------
# define bookmarking actions
#----------------------------------------------------------------------
# observe({
#     bm <- getModuleBookmark(id, module, bookmark, locks)
#     req(bm)
# })

#----------------------------------------------------------------------
# set return value
#----------------------------------------------------------------------
list(
    output = list()
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
