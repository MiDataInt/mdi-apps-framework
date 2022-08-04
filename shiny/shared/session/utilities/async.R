#----------------------------------------------------------------------
# MDI support for running asynchronous tasks as a wrapper around future_promise
#----------------------------------------------------------------------
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
