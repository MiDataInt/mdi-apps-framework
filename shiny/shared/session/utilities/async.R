#----------------------------------------------------------------------
# MDI support for running asynchronous tasks 
#----------------------------------------------------------------------

# as a wrapper around future_promise
mdi_async <- function(
    taskFn,             # a function that executes the asynchronous task; must not call reactives
    reactiveVal,        # reactiveVal (or a function with one argument) to monitor the task's progress and results
    name = "anonymous", # a string name for the task, used in error reporting and the results object
    default = NULL,     # the value passed to reactiveVal when taskFn fails
    promise = FALSE,    # if TRUE, return the task's promise, otherwise return NULL
    header = FALSE,     # if TRUE, feedback on the task progress and success is provided in the main page header
    async = TRUE,       # in select circumstances, caller may wish to force synchronous execution of the same task
    ...                 # additional arguments passed to taskFn
){
    # communicate the initiation of the intent to perform the task    
    if(header) headerStatus$initalizeAsyncTask(name, reactiveVal)
    reportTaskProgress <- function(pending, success = NULL, message = NULL, value = NULL){
        reactiveVal(list(
            name = name,     
            pending = pending, 
            success = success,
            message = message,
            value = value
        ))
    }
    reportTaskProgress(pending = TRUE)

    # initalize the promise and execution plan
    plan(if(async) multicore else sequential)
    p <- future_promise(taskFn(...)) %>% then(

        # return task metadata and result value on success
        onFulfilled = function(value) reportTaskProgress(    
            pending = FALSE, 
            success = TRUE, 
            value = value
        ), 

        # show an error dialog and return the default value on failure
        onRejected = function(e){
            if(!header) showUserDialog(
                title = "Asynchronous Task Error", 
                tags$p(paste("Asynchronous task", paste0("'", name, "'"), "reported the following error:")),
                tags$p(e$message, style = "padding-left: 10px; color: #600;"),
                tags$p("Proceeding with the default value."),
                type = "okOnly",
                size = if(nchar(e$message) > 200) "m" else "s"
            )
            reportTaskProgress(  
                pending = FALSE, 
                success = FALSE, 
                message = e$message,
                value = default
            )
        }
    )

    # usually return NULL, but return the promise object if requested
    if(promise) p else NULL
}

# using an observer with invalidateLater
setTimeout <- function(action, ..., delay = 500){
    timeHasElapsed <- FALSE
    jobId <- sample(1e8, 1)
    selfDestruct <- observe({
        if(timeHasElapsed){
            action(jobId, ...)
            selfDestruct$destroy()
        } else {
            timeHasElapsed <<- TRUE
            invalidateLater(delay)
        }
    })
    jobId
}
waitFor <- function(triggers, action, ..., delay = 100){ # triggers is a list of one or more reactives whose value must change for event to be fired
    if(!is.list(triggers)) triggers <- list(triggers)    
    initialValues <- sapply(triggers, function(x) x()) # should always be truthy, i.e., not NULL or NA
    timeHasElapsed <- FALSE
    jobId <- sample(1e8, 1)
    selfDestruct <- observe({
        if(timeHasElapsed && !any(sapply(triggers, function(x) x()) == initialValues)){
            action(jobId, ...)
            selfDestruct$destroy()
        } else {
            timeHasElapsed <<- TRUE
            invalidateLater(delay)
        }
    })
    jobId
}

# use setTimeout to step through a sequence of asynchronous load functions
#   loadData is whatever object/list is needed by loadSequence functions
#   loadSequence is the list of functions yet to execute
#   loadTriggers is an optional list of reactives of same length as loadSequence that provides triggers for waitFor, instead of setTimeout
# each function in loadSequence should recall doNextLoadSequenceItem()
doNextLoadSequenceItem <- function(loadData, loadSequence, delay = 100, loadTriggers = NULL){
    if(!isTruthy(loadSequence)) return() # self-terminate when no more functions to call
    nNext <- length(loadSequence)
    if(nNext > 1){
        if(is.null(loadTriggers)){
            setTimeout(loadSequence[[1]], loadData, loadSequence[2:nNext], delay = delay)
        } else if(!isTruthy(loadTriggers[[1]])) { # allow mixing of triggered and timed load events in one call
            setTimeout(loadSequence[[1]], loadData, loadSequence[2:nNext], loadTriggers[2:nNext], delay = delay)
        } else {
            waitFor(loadTriggers[[1]], loadSequence[[1]], loadData, loadSequence[2:nNext], loadTriggers[2:nNext], delay = delay)
        }
    } else{
        if(is.null(loadTriggers)){
            setTimeout(loadSequence[[1]], loadData, NA, delay = delay)
        } else if(!isTruthy(loadTriggers[[1]])) {
            setTimeout(loadSequence[[1]], loadData, NA, NA, delay = delay)
        } else {
            waitFor(loadTriggers[[1]], loadSequence[[1]], loadData, NA, NA, delay = delay)
        }
    }
}
