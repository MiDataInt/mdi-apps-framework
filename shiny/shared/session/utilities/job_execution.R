
#----------------------------------------------------------------------
# runs jobs on the server with different levels of priority and synchronicity
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# reactives related to job processing
#----------------------------------------------------------------------

# launch a job in response to user link click
addJobLaunchObserver <- function(
    session, input, module,
    data, launchJobId,
    sendFeedback,
    statusChange    
){
    analyzeName <- appStepNamesByType$analyze
    observeEvent(input[[launchJobId]], {
        reportProgress(launchJobId, module)
        selectedRow <- getTableActionLinkRow(input, launchJobId)
        schemaId <- names(data$list)[selectedRow]
        
        # check once more that we don't have a stale job status
        ##################
        #unlink(getJobStatusFile(schemaId), force=TRUE)
        diskStatus <- getJobDiskStatus(schemaId)
        if(!is.null(diskStatus) && diskStatus > CONSTANTS$jobStatuses$created$value){
            return(setJobStatus(statusChange, schemaId, list(status = diskStatus)))
        }          

        # learn how to run the job from an analysis type and specific schema        
        reportProgress(paste(selectedRow, '=', schemaId))
        schema <- data$list[[selectedRow]]
        analysisType <- app[[analyzeName]]$analysisTypes[[schema$Analysis_Type]]
        jobType <- analysisType$jobType
        job <- structure(
            list(
                schemaId = schemaId,
                schema = schema,
                analysisType = analysisType
            ),
            class = schema$Analysis_Type
        )
        job <- setJobParameters(job) # uses S3 generic specific to schema$Analysis_Type

        # launch the job
        reportProgress(paste('launchJob', jobType), module)
        setJobStatus(statusChange, schemaId, list(status=CONSTANTS$jobStatuses$running$value))    
        launcher <- if(jobType == "immediate")     runJobImmediately
                    else if (jobType == "promise") runJobWithPromise
                    else if (jobType == "hpc")     sendJobToHPC
                    else stop(safeError('unknown job type'))
        launcher(session, job, statusChange)
        
        # if asynchronous, provide feedback on progress to user
        if(!is.null(sendFeedback) && jobType != "immediate") sendFeedback("job launched successfully")
    })
}

# signal the analysis status change to app
addStatusChangeObserver <- function(module, data, statusChange){
    observeEvent(statusChange(), {
        req(statusChange())
        list <- statusChange() # expected to a have a single-element named list
        schemaId <- names(list)
        newStatus <- list[[schemaId]]
        reportProgress(paste('statusChange', schemaId, newStatus), module)
        data$list[[schemaId]]$status <- newStatus # cascades to update summary table
    })
}
setJobStatus <- function(statusChange, schemaId, jobOutput){
    list <- list()
    list[[schemaId]] <- jobOutput$status
    write(jobOutput$status, getJobStatusFile(schemaId, create=TRUE), append=FALSE)
    statusChange(list)
}
getJobDiskStatus <- function(schemaId){
    statusFile <- getJobStatusFile(schemaId)
    if(!file.exists(statusFile)) return(NULL)
    as.integer(loadResourceText(statusFile))
}

#----------------------------------------------------------------------
# different modes of job execution
#   these functions must update job status, in parent process, after child resolves
#----------------------------------------------------------------------

# execute work immediately in the user's main web server process
#   suitable for very fast jobs (<5 sec) with no danger of ballooning
runJobImmediately <- function(session, job, statusChange){
    fn <- 'runJobImmediately'
    startSpinner(session, fn)
    jobOutput <- executeJob(job) # synchronous execution
    stopSpinner(session, fn)
    setJobStatus(statusChange, job$schemaId, jobOutput)
}

# execute work on the web server, but in a new R process using promises
#   suitable for moderately fast/complex jobs (<5 min) that won't crash the web server
runJobWithPromise <- function(session, job, statusChange){
    fn <- 'runJobWithPromise'
    startSpinner(session, fn)
    future({
        #----------------------------------------------------------
        # RUNS IN CHILD PROCESS
        #----------------------------------------------------------
        tryCatch({
            isParentProcess <<- FALSE
            jobErrorType <<- CONSTANTS$jobErrorTypes$scriptLoading            
            childEnv <- environment()
            output <- list() # allows catch of a Globus UI output definition in base scripts
    
            # initialize the child process to match the parent process
            reportProgress('loading framework scripts')
                setwd(serverEnv$SHARED_DIR) # just in case...
                source(file.path('global','packages','packages.R'), local=childEnv) # order is important here          
                source('global.R', local=childEnv)
                loadAllRScripts('global', recursive=TRUE, local=childEnv)
                loadAppScriptDirectory('session', local=childEnv)
            reportProgress('loading app scripts', app$info$name)
                loadAppScriptDirectory(app$DIRECTORY, local=childEnv)
            reportProgress('loading analysis scripts', job$schema$Analysis_Type)
                analysisTypeDir <- Sys.glob( file.path('optional','types','analysisTypes','*',job$schema$Analysis_Type) )     
                loadAppScriptDirectory(analysisTypeDir, local=childEnv) 

            # run the target job and return its status
            reportProgress('configuring the job', job$schema$Analysis_Type)
            jobErrorType <<- CONSTANTS$jobErrorTypes$jobConfiguration            
                executeJob(job)

        # catch job errors that arose prior to the call to tryCatchJob
        # errors while executing the job itself are caught and reported by tryCatchJob
        }, error=function(error){
            if(!exists('jobErrorType')) jobErrorType <- CONSTANTS$jobErrorTypes$futureEval
            reportProgress(jobErrorType, 'ERROR')
            reportProgress(error, 'ERROR')
            list(
                status  = CONSTANTS$jobStatuses$failure$value,
                message = error,
                type    = jobErrorType
            ) 
        })
        #----------------------------------------------------------

    # strongly coded declaration of global/session values required by child processes
    }, seed=TRUE, globals = c(
                   
        # globals
        'reportProgress', # used immediately in child, before scripts are sourced
        'serverEnv',      # used to source scripts
        'CONSTANTS',      # could be used if child dies immediately

        # server level variables
        'globusConfig',

        # variables and functions defined in server function for this session
        'dataDirs', 
        'app', # for static values ONLY; reactives must be converted to static values in jobParameters
        'appStepNamesByType',
        'manifestTypes',
        'uploadTypes',
        'inlineScripts',
        'loadAllRScripts',
        'loadAppScriptDirectory',
            
        # variables that define the specific work requested by the user
        'job'

    # return the results of the promise to the parent process by setting the job status flag
    )) %...>% (function(jobOutput) {
        setJobStatus(statusChange, job$schemaId, jobOutput)
    })
    stopSpinner(session, fn) # main spinner stops when job has been sent to async execution
}

# queue job to a higher performance compute server
#   suitable for longish jobs (<30 min) within our willingness to support
sendJobToHPC <- function(session, job, statusChange){  
    reportProgress('sendJobToHPC')
    # pending: this may be achieveble using future in any case
}

#----------------------------------------------------------------------
# return a common set of reactive-to-static conversions used by many analyses
#----------------------------------------------------------------------
reactiveToStatic_commmon <- function(){
    reportProgress('reactiveToStatic_commmon')
    x <- list()
    for(stepType in c('upload','assign')){
        stepName <- appStepNamesByType[[stepType]]
        outcomes <- app[[stepName]]$outcomes
        for(outcomeName in names(outcomes)){
            x[[outcomeName]] <- outcomes[[outcomeName]]() # execute the reactive to obtain current, static values
        }
    }
    x
}

#----------------------------------------------------------------------
# execute a job (finally!)
#----------------------------------------------------------------------
#   runs in child process (e.g. with promise), i.e. after launch
#       print output to disk
#       return a completion status and message
#----------------------------------------------------------------------
tryCatchJob <- function(expr, schemaId){
    #----------------------------------------------------------
    # RUNS IN CHILD PROCESS
    #----------------------------------------------------------
    reportProgress('executing the job')
    jobOutput <- tryCatch({
        results <- eval(expr)
        list(status  = CONSTANTS$jobStatuses$success$value,
             message = "OK",
             results = results) 
    }, warning = function(warning) {
        reportProgress(CONSTANTS$jobErrorTypes$jobExecution, 'WARNING')
        reportProgress(warning, 'WARNING')
        list(status  = CONSTANTS$jobStatuses$warning$value,
             message = warning,
             type    = CONSTANTS$jobErrorTypes$jobExecution) 
    }, error = function(error) {
        reportProgress(CONSTANTS$jobErrorTypes$jobExecution, 'ERROR')
        reportProgress(error, 'ERROR')
        list(status  = CONSTANTS$jobStatuses$failure$value,
             message = error,
             type    = CONSTANTS$jobErrorTypes$jobExecution) 
    })

    # write job output to disk with results and job status information
    #   always use permanent storage so that results can be recalled in future app loads
    reportProgress('saving job results')    
    #save(jobOutput, file=getJobRDataFile(schemaId, create=TRUE))
    saveRDS(jobOutput, file=getJobRdsFile(schemaId, create=TRUE))
    reportProgress('job done')    
    
    # return just the status and any error messages (not the job results)
    jobOutput$results <- NULL
    jobOutput
    #----------------------------------------------------------
}

