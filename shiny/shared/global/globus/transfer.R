
#----------------------------------------------------------------------
# functions to schedule and monitor Globus file transfer actions
#----------------------------------------------------------------------

# submit the file transfer request, for just one item
transferGlobusFile <- function(source, destination, label="MAGC Portal transfer"){
    
    # STEP 1: get a submission id
    submissionId <- getGlobusTransfer('submission_id') 
    if(!submissionId$success) return(submissionId)    
    
    # STEP 2: request the tranfer and return the response (on request success or failure)
    body <- list(
        "DATA_TYPE" =  "transfer",
        "submission_id" =  submissionId$contents$value,
        "source_endpoint" =  source$endpoint,
        "destination_endpoint" =  destination$endpoint,
        "label" =  label,
        "sync_level" = NULL,
        "notify" = "off",
        "DATA" =  list(
            list(
                "source_path" =  source$path,
                "destination_path" =  destination$path,
                "recursive" =  FALSE,
                "DATA_TYPE" =  "transfer_item"
            )
        )
    )
    postGlobusTransfer('transfer?fields=code,task_id', body) 
}

# monitor the status of a transfer
launchTransferTaskObserver <- function(session, transfer, success, failure, intervalMs=5000){
    startSpinner(session, 'globus launchTransferTaskObserver')
    counter <- 0
    statusObserver <- observe({
        invalidateLater(intervalMs) # default checks every 5 seconds, small files go fast once initiated
        counter <<- counter + 1 
        if(counter == 1) return() # give Globus a break and don't check until one interval has passed
        status <- getGlobusTransferSuccess(transfer)
        if(is.null(status$success)) return() # transfer still in progress, try again later
        statusObserver$destroy() # end the loop
        stopSpinner(session)        
        if(status$success) success(status) else failure(status) # engage the callback
    })    
}
getGlobusTransferSuccess <- function(transfer){
    if(!transfer$success) return(transfer)
    request <- paste(
        file.path('task', transfer$contents$task_id),
        'fields=status,fatal_error,nice_status,nice_status_short_description',
        sep='?'
    )
    task <- getGlobusTransfer(request)
    if(!task$success) return(task)
    list(
        success = switch(task$contents$status,
            ACTIVE    = NULL,  # task is in process, no status yet
            INACTIVE  = FALSE, # task requires intervention (we consider it failed)
            SUCCEEDED = TRUE,  # task completed
            FAILED    = FALSE  # task failed irrevocably
        ),
        contents = task$contents
    )
}


# TRANSFER RESPONSE
#$success # SET BY US
#[1] TRUE
#$contents # PROVIDED BY GLOBUS
#$contents$DATA_TYPE
#[1] "transfer_result"
#$contents$code
#[1] "Accepted"
#$contents$message
#[1] "The transfer has been accepted and a task has been created and queued for execution"
#$contents$request_id
#[1] "VaXTmVqhf"
#$contents$resource
#[1] "/transfer"
#$contents$submission_id
#[1] "83c91aa1-804e-11eb-a932-81bbe47059f4"
#$contents$task_id
#[1] "83c91aa0-804e-11eb-a932-81bbe47059f4"
#$contents$task_link
#$contents$task_link$DATA_TYPE
#[1] "link"
#$contents$task_link$href
#[1] "task/83c91aa0-804e-11eb-a932-81bbe47059f4?format=json"
#$contents$task_link$rel
#[1] "related"
#$contents$task_link$resource
#[1] "task"
#$contents$task_link$title
#[1] "related task"

