#--------------------------------------------------------------
# define global constants
#--------------------------------------------------------------
# wrap these values in CONSTANTS list as an explicit reminder 
# in code that no app should ever change these values
#--------------------------------------------------------------
CONSTANTS <- list(
    
    # file size limits
    maxFileTransferSize = 10 * 1e6,
    maxFileTransferSizeMegaBytes = 10,

    # git constants
    mainBranch      = 'main',
    originRemote    = 'origin', # the name of the remote parent of the local clone
    upstreamRemote  = 'upstream', # for forks, the name of the grandparent of the local clone, i.e. the definitive repo

    # definitions of the file types we accept for upload
    sourceFileTypes = list(
        manifest  = 'manifest',  # metadata on a collection of samples suitable for a Stage 1 pipeline
        package   = 'package',   # the output of a Stage 1 pipeline suitable for loading into a Stage 2 app
        dataTable = 'dataTable', # a flat file of data to load directly into an app (bypassing any Stage 1 pipeline)
        bookmark  = 'bookmark',  # a file saved previously by a user working in an app, contains page states but no data
        book      = 'book'       # a bookmark together with all package files required to run it (fully transportable)
    ),
    fileSuffixes = list(
        manifest  = c('.csv'),
        package   = c('.mdi.package.zip'),        
        dataTable = c('data.csv'), 
        bookmark  = c('.mdi'),
        book      = c('.mdi.bookmark.zip')
    ),
    
    # the name of the app for running Stage 1 pipelines and other framework functions
    apps = list(
        loginPage      = 'login-page',
        launchPage     = 'launch-page',
        serverBusy     = 'server-busy',
        pipelineRunner = 'pipelineRunner'
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
        statusFile   = 'statusFile',   # the Stage 1 pipeline output status (optional)
        qcReport     = 'qcReport'      # a file, typically html or PDF, with upstream QC analysis results (optional)
    ),

    # sample manifest columns used to identify ALL samples, always (regardless of manifest type)
    manifestKeyColumns = c('Project', 'Sample_ID', 'Description'),
    
    # storage kyes
    bookmarkKey = 'bookmarks',
    autoSavedBookmark = 'auto saved',
    
    # first item in select boxes to force user to make a selection
    nullSelectSetOption = list("-----" = ""), 
    
    # where in the execution chain an analysis job is at the present time
    jobStatuses = list(
        created = list(value = -2, icon = NULL),
        running = list(value = -1, icon = as.character(icon("circle-notch", class = "fa-spin"))),
        success = list(value =  0, icon = as.character(icon("check-circle"))),
        warning = list(value =  1, icon = as.character(icon("times-circle"))), # insist on no warnings either
        failure = list(value =  2, icon = as.character(icon("times-circle")))
    ),
    
    # exactly how an analysis job failed
    jobErrorTypes = list( 
        futureEval       = 'while evaluating the future code block in runJobWithPromise',
        scriptLoading    = 'during script loading in the child process, prior to call to executeJob',
        jobConfiguration = 'during job configuration by executeJob, after script loading in the child process',
        jobExecution     = 'during execution of the job by tryCatchJob in the child process'
    )    
    
)
