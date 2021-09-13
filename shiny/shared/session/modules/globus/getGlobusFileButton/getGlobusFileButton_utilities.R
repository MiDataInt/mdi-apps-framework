#----------------------------------------------------------------------
# validate and schedule the transfer of a file that a user
# has requested to import for analysis (instead of uploading it)
#----------------------------------------------------------------------

# master sub coordinating the file import process
importFromGlobus <- function(target, state){
    target$name <- target[['file[0]']]
    target <- checkGlobusImport(target, state)
    confirmGlobusImport(target,
        success = function(status){
            name <- target$name
            path <- file.path(globusConfig$endpoint$localPath, name)
            if(app$NAME == CONSTANTS$apps$launchPage) {
                file <- list(name=name, datapath=path)                   
                loadIncomingFile(file, state$allowedFileTypes, throwGlobusImportError)            
            } else {
                file <- list(name=name, path=path, type=getIncomingFileType(name))         
                firstStep <- app[[ names(app$info$appSteps)[1] ]]        
                firstStep$loadSourceFile(file)                  
            }   
        }, 
        failure = function(status){
            throwGlobusImportError(HTML(paste(
                'File transfer failed',
                status$contents$code,
                status$contents$message,
                sep = "<br>"
            )))
            #status$contents$nice_status
            #status$contents$nice_status_short_description
        })
}

# validate the requested incoming file
checkGlobusImport <- function(target, state){
    
    # must be a single file (not a folder)
    if(is.null(target$endpoint_id) ||
       is.null(target$path) ||
       is.null(target$name)) req(FALSE) # do not throw an error if there is no file
    if(!is.null(target[['file[1]']])) {
        throwGlobusImportError('Please select a single file for transfer.')
    }
    
    # file type must be allowed in the load context
    if(!isAllowedSourceFileType(target$name, state$allowedFileTypes)){
        throwGlobusImportError('Unknown or unsupported file type.')
    }

    # get and check the file size (reject overly large files)
    fileQuery <- paste0('ls?path=', target$path,
                        '&filter=name:', target$name,
                        '&fields=size,last_modified')
    file <- getGlobusTransfer(file.path('operation/endpoint', target$endpoint_id, fileQuery))
    if(!file$success) throwGlobusImportError('Failed to retrieve file metatdata from Globus.')
    file <- file$contents$DATA[[1]]
    if(file$size > CONSTANTS$maxFileTransferSize )
        throwGlobusImportError(paste('Max file transfer size exceeded. Files must be smaller than',
                                     CONSTANTS$maxFileTransferSizeMegaBytes, 'MB.'))
    # return the extended file information
    c(target, file)
}

# confirm the file import with the user
# among other things, this allows apps to reload during confirmation
confirmGlobusImport <- function(target, success, failure){
    showUserDialog(
        'Confirm Globus Transfer',
        tags$p('Click OK to confirm transfer of the following file from Globus to the MAGC Portal:'),
        tags$p(
            style = "margin-left: 25px;",
            target$endpoint,
            tags$br(),
            tags$br(),
            target$path,
            tags$br(),
            target$name,
            tags$br(),
            tags$br(),
            paste(format(target$size, big.mark=",", scientific=FALSE) , 'bytes'),
            tags$br(),
            paste('last modified', target$last_modified)
        ),
        callback=function(x){
            transfer <- scheduleGlobusImport(target)
            launchTransferTaskObserver(session, transfer, success, failure)  
        },
        easyClose=FALSE,
        size="m"
    )    
}

# make the incoming file transfer request
scheduleGlobusImport <- function(target){
    transferGlobusFile(
        source = list(
            endpoint = target$endpoint_id,
            path = file.path(target$path, target$name)
        ),
        destination = list(
            endpoint = globusConfig$endpoint$id, # a staging location, will be removed quickly 
            path = target$name
        ),
        label = 'MAGC Portal import' # do not use file name, it often has disallowed characters
    )
}

# catch all error feedback for Globus file import
throwGlobusImportError <- function(msg, isError=TRUE){
    if(!is.null(msg) && msg != "" && isError) showUserDialog(
        'Globus File Error',
        tags$p(msg),
        easyClose=TRUE,
        type='okOnly',
        size="s"
    )
    req(!isError) # stop all further execution
}

