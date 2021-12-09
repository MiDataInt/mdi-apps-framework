#----------------------------------------------------------------------
# reactive components to launch a pipeline job
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
# honor the refresh button
#----------------------------------------------------------------------
refreshStatus <- reactiveVal(0)
refreshOutput <- reactiveVal(0)
observeEvent(input$refresh, {
    if(is.na(selectedJob()))
        refreshStatus( refreshStatus() + 1 )
    else
        refreshOutput( refreshOutput() + 1 )
})

#----------------------------------------------------------------------
# cascade to show the status of the selected job configuration file
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
    refreshStatus()
    jobFile <- activeJobFile()
    req(jobFile)

    # write the status to file using mdi status (but don't use it's return value)
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
# cascade to show the status of the selected job
#----------------------------------------------------------------------
selectedJob <- rowSelectionObserver('statusTable', input)
observe({
    refreshOutput()
    jobFile <- activeJobFile()
    req(jobFile)
    statusTable <- statusTable()
    req(statusTable)
    rowI <- selectedJob()
    req(rowI)
    jobId <- statusTable[rowI, jobID]
    req(jobId)
    startSpinner(session, 'mdi report')
    args <- c('report', '-j', jobId, jobFile$path)
    x <- runMdiCommand(args)
    req(x$success)
    stopSpinner(session, 'mdi report')
    outputData(list(
        type = "report",
        text = x$results
    ))
})

#----------------------------------------------------------------------
# dry run and live submit of jobs
#----------------------------------------------------------------------
observeEvent(input$dryRun, {
    jobFile <- activeJobFile()
    req(jobFile)
    startSpinner(session, 'mdi dry run')
    finishUp <- function(data){
        outputData(list(
            type = "dryRun",
            text = data
        ))
        stopSpinner(session, 'mdi dry run')        
    }

    # # pipeline dry run, reports all option values
    # args <- c(jobFile$pipeline, jobFile$path, '--dry-run')
    # x1 <- runMdiCommand(args)
    # if(!x1$success) return( finishUp(x1$results) )

    # finishUp(x1$results)



    # job manager dry run, reports the job list
    args <- c('submit', '--dry-run', jobFile$path)

return( finishUp(paste(args, collapse = "\n") ))

    x2 <- runMdiCommand(args)
    if(!x2$success) return( finishUp(x2$results) )

    finishUp(x2$results)

    # # combine the two reports in out display
    # finishUp(paste(
    #     x2$results,
    #     # "\n",
    #     # x1$results,
    #     sep = "\n"
    # ))
})


#----------------------------------------------------------------------
# render all command outputs (except)
#----------------------------------------------------------------------
outputData <- reactiveVal(list(
    type = "",
    text = NULL
))
output$output <- renderText({
    outputData()$text
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
