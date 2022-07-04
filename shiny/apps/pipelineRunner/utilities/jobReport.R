# utilities for processing job reports

# return the aggregated YAML blocks from the job report as a nested R list
parsePipelineJobReport <- function(jobId, jobFile){
    req(jobId)
    req(jobFile)
    lines <- runMdiCommand(args = c("report", "-j", jobId, jobFile$path), collapse = FALSE)
    if(!lines$success) return(NULL)
    lines <- lines$results

    # process the report as a yaml stream with multiple blocks, a.k.a. documents
    blockStartI <- NULL
    blockEndI <- NULL 
    report <- list(
        jobManager = list(),
        options = list(),
        tasks = list()
    )   
    parseReportBlock <- function(){
        if(is.null(blockStartI) || is.null(blockEndI) || blockEndI - blockStartI < 1) return(NULL)
        read_yaml(text = paste(lines[blockStartI:blockEndI], collapse = "\n"))
    }    
    for(i in seq_along(lines)){
        if(lines[i] == "---" || lines[i] == "...") {
            blockEndI <- i - 1
            block <- parseReportBlock()
            if(!is.null(block)) { # merge the various job-manager portions of the YAML stream
                if(!is.null(block[['job-manager']])) report$jobManager <- c(report$jobManager, block[['job-manager']])
                if(!is.null(block$execute)) report$options <- block
                if(!is.null(block$task)) report$tasks[[length(report$tasks) + 1]] <- block$task
            }
        } 
        if(lines[i] == "---") blockStartI <- i + 1
        if(lines[i] == "...") blockStartI <- NULL
    }  
    report
}

# parse just the pipeline name from format suite/pipeline:version
getShortPipelineName <- function(pipeline){
    if(is.null(pipeline)) return(NULL)
    pipeline <- if(grepl("/", pipeline)) strsplit(pipeline, "/")[[1]][2] else pipeline
    strsplit(pipeline, ":")[[1]][1] # just the pipeline name (no suite or version) 
}

# get the option value(s) that apply to the specific task(s) in a report
getTaskOptions <- function(report, family, option){
    action <- report$options$execute
    options <- report$options[[action]]
    values <- options[[family]][[option]]
    if(length(values) <= 1) return(values)
    sapply(report$tasks, function(task) values[task[['task-id']]])
}
