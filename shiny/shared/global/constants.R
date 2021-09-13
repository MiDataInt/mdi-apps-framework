
#--------------------------------------------------------------
# define global constants
#--------------------------------------------------------------
# wrap these values in a list as an explicit reminder in code that
# no app should ever change these values
#--------------------------------------------------------------
CONSTANTS <- list(
    
    # file size limits
    maxFileTransferSize = 10 * 1e6,
    maxFileTransferSizeMegaBytes = 10,

    # git constants
    mainBranch      = 'main',
    developerBranch = 'develop',
    defaultEditBranch = 'framework',
    originRemote    = 'origin', # the name of the remote parent of the local clone
    upstreamRemote  = 'upstream', # for forks, the name of the grandparent of the local clone, i.e. the definitive repo

    # definitions of the file types we accept for upload
    sourceFileTypes = list(
        manifest  = 'manifest',
        project   = 'project',
        dataTable = 'dataTable',
        bookmark  = 'bookmark'
    ),
    fileSuffixes = list(
        manifest  = c('.csv'),
        project   = c('.magc.package.zip'),        
        dataTable = c('.csv'), 
        bookmark  = c('.magc')
    ),
    
    # the name of the app for running magc-portal-pipelines
    apps = list(
        launchPage     = 'launch-page',
        serverBusy     = 'server-busy',
        pipelineRunner = 'pipeline-runner'
    ),
    
    # the standardized content file types found in project zips
    # names in this list are the names in yml, e.g.
    #    files:
    #        manifestFile:
    #            file: xxx.csv
    #            type: manifest-file
    #            manifestType: XYZ
    # other file types are allowed also, these types are examined at first load of project file
    contentFileTypes = list(
        manifestFile = 'manifestFile', # the sample manifest for the project (required)
        statusFile   = 'statusFile',   # the stage 1 pipeline output status (optional)
        qcReport     = 'qcReport'      # a file, typically html or PDF, with upstream QC analysis results (optional)
    ),

    # sample manifest columns used to identify ALL samples, always (regardless of manifest type)
    manifestKeyColumns = c('Project','Sample_ID','Description'),
    
    # storage kyes
    bookmarkKey = 'bookmarks',
    autoSavedBookmark = 'auto saved',
    
    # first item in select boxes to force user to make a selection
    nullSelectSetOption = list("-----" = ""), 
    
    # where in the execution chain an analysis job is at the present time
    jobStatuses = list(
        created = list(value = -2, icon=NULL),
        running = list(value = -1, icon=as.character(icon("circle-notch", class="fa-spin"))),
        success = list(value =  0, icon=as.character(icon("check-circle"))),
        warning = list(value =  1, icon=as.character(icon("times-circle"))), # insist on no warnings either
        failure = list(value =  2, icon=as.character(icon("times-circle")))
    ),
    
    # exactly how an analysis job failed
    jobErrorTypes = list( 
        futureEval       = 'while evaluating the future code block in runJobWithPromise',
        scriptLoading    = 'during script loading in the child process, prior to call to executeJob',
        jobConfiguration = 'during job configuration by executeJob, after script loading in the child process',
        jobExecution     = 'during execution of the job by tryCatchJob in the child process'
    )    
    
)

