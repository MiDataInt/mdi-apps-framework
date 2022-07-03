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

# DEVELOPER ACTION, not as relevant to most users except maybe to poke into data file (samtools view...)
#   shell          open a command shell in a pipeline action's runtime environment
# mdi pipeline shell --action --runtime [command]

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
                                 errorString = 'mdi error:', options = ""){
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
status <- asyncTableServer("status", function(jobFile){

    # call 'mdi status' to update status files, but use the files, not the mdi return value
    x <- runMdiCommand(c('status', jobFile$path), collapse = FALSE)
    if(!x$success) return(nullStatusTable)

    # recover the tab-delimited status text from disk
    dataDir <- paste0(".", jobFile$name, ".data")
    statusFile <- paste0(jobFile$name, ".status")
    statusFile <- file.path(jobFile$directory, dataDir, statusFile)
    if(!file.exists(statusFile)) return(nullStatusTable) 

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
            allow = !isTerminated(x$exit_status)
        )  
    )  
    cbind(y, x)
}, options = list(
    paging = FALSE,      
    searching = FALSE            
))
statusTableObserver <- observeEvent({
    input$refreshStatus
    invalidateStatusTable()
    activeJobFile()
}, {
    jobFile <- activeJobFile()
    req(jobFile) 
    status$update(jobFile = jobFile, default = nullStatusTable)
})

#----------------------------------------------------------------------
# enable job deletion from within the status table
#----------------------------------------------------------------------
deleteLinkId <- 'deleteLink'
observeEvent(input[[deleteLinkId]], {
    statusTable <- status$tableData()
    req(statusTable)
    req(!statusTable$pending)
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
selectedJobI <- status$table$rows_selected
selectedJob <- reactive({
    rowI <- selectedJobI()
    req(rowI)
    status$tableData()[rowI, ]
})
selectedJobId <- reactive({
    job <- selectedJob()
    req(job)
    job[, jobID]
})
isTerminated <- Vectorize(function(exit_status) exit_status == "deleted" || check.numeric(exit_status))
isTerminatedJob <- reactive({
    job <- selectedJob()
    req(job)
    job[, isTerminated(exit_status)]
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
    if(is.na(tasks) || tasks == "") "" else tagList(
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
    req(!isTerminatedJob())
    column(
        width = 2,
        bsButton(session$ns("top"), "Process Metrics", style = "default", width = "100%")
    )
})
output$taskOptions <- renderUI({
    job <- selectedJob()
    req(job)
    req(job[, jobName != "no submitted jobs"])
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
expandedJobId <- reactiveVal(NULL)
runTaskLevelCommand <- function(command, fn = NULL){
    jobId <- selectedJobId()
    req(jobId)
    tasks <- selectedJob()[, array]
    taskNumber <- if(is.na(tasks)) NA else input$taskNumber
    jobId <- paste0(jobId, if(is.null(taskNumber) || is.na(taskNumber) || taskNumber == allTasks){
        ""
    } else {
        paste0("[", taskNumber, "]")
    })
    expandedJobId(jobId)
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
results <- reactive({ # command output as an array of lines
    data <- outputData()$data
    req(data)
    req(data$results)
    strsplit(data$results, "\n")[[1]]
})
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
# build all required conda environments and/or download Singularity containers
#----------------------------------------------------------------------
missingCondaPipelines <- reactiveVal(NULL)
missingImages <- reactiveVal(list())
buildEnvironmentButton <- function(){ # must come before executeButtonMetadata
    results <- results()
    req(results)
    blockStarts <- which(results == "---") # boundaries of YAML blocks, a.k.a documents
    blockEnds   <- which(results == "...")
    missingCondaPipelines_ <- list()
    missingImages_ <- list()
    for(i in seq_along(blockStarts)){
        yaml <- read_yaml(text = paste0(results[blockStarts[i]:blockEnds[i]], collapse = "\n"))
        action <- yaml$execute
        if(is.null(action)) next # false for task and other non-job-level YAML blocks
        pipeline <- if(grepl("/", yaml$pipeline)) strsplit(yaml$pipeline, "/")[[1]][2] else yaml$pipeline
        pipeline <- strsplit(pipeline, ":")[[1]][1] # just the pipeline name (no suite or version)
        singularity <- yaml[[action]]$singularity
        if(is.null(singularity)){
            conda <- strsplit(yaml[[action]]$conda$prefix, "\\s+")[[1]][1]
            if(!dir.exists(conda)) missingCondaPipelines_[[pipeline]] <- pipeline
        } else { # both the host system and the pipeline support containers and runtime = auto or container
            image <- rev(strsplit(singularity$image, "/")[[1]])
            imageFile <- file.path(serverEnv$MDI_DIR, "containers", image[2], pipeline, paste0(sub(":v", "-v", image[1]), ".sif"))
            if(!file.exists(imageFile)) missingImages_[[singularity$image]] <- pipeline
        }
    }       
    missingCondaPipelines(missingCondaPipelines_)
    missingImages(missingImages_)
    if(length(missingCondaPipelines_) > 0 || length(missingImages_) > 0){
        bsButton(session$ns('environments'), "Build/Download Environment(s)", style = "primary", width = "100%")
    } else ""
}
buildEnvironments <- function(jobFile, missingCondaPipelines, missingImages){
    results <- c()
    if(length(missingCondaPipelines) > 0) for(pipeline in unique(unlist(missingCondaPipelines))){
        results <- c(results, runMdiCommand(args = c(pipeline, "conda", "--create", "--force"))$results)
    }
    if(length(missingImages) > 0) for(pipeline in unique(unlist(missingImages))){
        results <- c(results, runMdiCommand(args = c(pipeline, "checkContainer", jobFile))$results)
    }
    paste(results, collapse = "\n\n")
}
observeEvent(input$environments, {
    jobFile <- activeJobFile()
    missingCondaPipelines <- missingCondaPipelines()
    missingImages <- missingImages()
    showUserDialog(
        title = "Confirm Asynchronous Build",
        tags$p("The following execution environments will be built or downloaded asynchronously.",
               "You may keep working until the process completes and then submit your jobs."),
        if(length(missingCondaPipelines) > 0) tagList(
            tags$p(tags$strong("Conda Environments"), style = "margin-bottom: 0;"),
            lapply(names(missingCondaPipelines), tags$p, style = "padding-left: 2em;")
        ) else "",
        if(length(missingImages) > 0) tagList(
            tags$p(tags$strong("Singularity Containers"), style = "margin-bottom: 0;"),
            lapply(names(missingImages), tags$p, style = "padding-left: 2em;")
        ) else "",
        callback = function(...) mdi_async(
            buildEnvironments,
            reactiveVal(""), # a new reactive for this task  
            name = "buildEnvironments",
            header = TRUE,
            jobFile = jobFile,
            missingCondaPipelines = missingCondaPipelines,
            missingImages = missingImages
        ),
        size = "m"
    )
})

#----------------------------------------------------------------------
# download a data package from a job for use in Stage 2 apps
#----------------------------------------------------------------------
packageFile <- reactiveVal(NULL)
downloadPackageButton <- function(){ # must come before executeButtonMetadata
    packageFile(NULL)
    results <- results()
    req(results)
    is <- which(grepl("writing Stage 2 package file", results))
    req(is)
    i <- max(is)
    req(i)
    i <- i + 1 # this line carries the name of the most recently written data package.zip
    packageFile(results[i])
    downloadButton(session$ns('download'), label = "Download Package", 
                   icon = NULL, width = "100%", # style mimics bsButton style=primary
                   style = "color: white; background-color: #3c8dbc; width: 100%; border-radius: 3px; float: right;")
}
output$download <- downloadHandler(
    filename = function() basename(packageFile()),
    content  = function(tmpFile) file.copy(packageFile(), tmpFile)
)

#----------------------------------------------------------------------
# open a terminal emulator in an output directory ...
#----------------------------------------------------------------------
# ... one the server/login node
showOutputDirTerminal <- function(ssh = FALSE){ # must come before executeButtonMetadata
    if(ssh){
        jobId <- expandedJobId()        
        jobFile <- activeJobFile() # collect job metadata from log report
        x <- runMdiCommand(args = c("report", "-j", jobId, jobFile$path), collapse = FALSE)
        if(!x$success) return(NULL)
        x <- sapply(c('host:', 'output-dir:', 'data-name:'), function(option){
            i <- which(grepl(option, x$results))[1] # mdi metadata always before job-specific logs
            trimws(strsplit(x$results[i], option)[[1]][2])
        })
        dir <- paste(x[2:3], collapse = "/")
        host <- x[1]
        req(host)
    } else {
        results <- results()
        req(results)
        dir <- results[1]
        host <- NULL
    }
    req(dir)
    showCommandTerminal(
        session, 
        user = headerStatusData$userDisplayName, 
        dir = dir,
        forceDir = TRUE,
        host = host
    )
}
# ... on the node running the task
showNodeDirTerminal <- function(){ # must come before executeButtonMetadata
    showOutputDirTerminal(ssh = TRUE)
}

#----------------------------------------------------------------------
# enable final execution, i.e., after (from within) a --dry-run display
#----------------------------------------------------------------------
executeButtonMetadata <- list(
    inspect = list(
        button = buildEnvironmentButton,
        execute = function(...) NULL
    ),
    mkdir = list(
        label = "Make Directory", 
        style = "primary", 
        suppressIf = "all output directories already exist"
    ),
    submit = list(
        label = "Execute Submit",         
        style = "success"
    ),
    extend = list(
        label = "Execute Extend",         
        style = "success"
    ),
    rollback = list(
        label = "Execute Rollback",       
        style = "danger"
    ),
    purge = list(
        label = "Execute Purge",          
        style = "danger"
    ),
    report = list(
        button = downloadPackageButton,
        execute = function(...) NULL
    ),
    ls = list(
        label = "Open in Terminal",          
        style = "default",
        execute = showOutputDirTerminal
    ),
    top = list(
        label = "Open Node in Terminal",          
        style = "default",
        execute = showNodeDirTerminal
    )
)
output$executeButton <- renderUI({ # render the Execute ... button
    command <- outputData()$command
    req(command)  
    d <- executeButtonMetadata[[command]]   
    req(d)     
    if(!is.null(d$button)) return(d$button()) # handle button overrides
    if(!is.null(d$suppressIf)){ # handle conditional button display
        data <- outputData()$data
        req(data)
        results <- data$results
        req(results)
        if(any(grepl(d$suppressIf, results))) return(NULL)
    }
    bsButton(session$ns('execute'), d$label, style = d$style, width = "100%")
})
observeEvent(input$execute, { # act on the Execute button click
    command <- outputData()$command
    req(command)
    d <- executeButtonMetadata[[command]] 
    if(!is.null(d$execute)) return(d$execute()) # handle button overrides 
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
