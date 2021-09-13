
#----------------------------------------------------------------
# app code development tools
#----------------------------------------------------------------

# report code steps to console
reportProgress <- function(msg, module=NULL){
    if(serverEnv$DEBUG){
        if(!is.null(module) && module != '') msg <- paste0(module, ": ", msg)
        if(!isParentProcess) msg <- paste0('CHILD PROCESS', ": ", msg)
        message(msg)
    }
}

# report how long a specific step is taking
RCodeTimer <- NULL
startRCodeTimer <- function() {
    RCodeTimer <<- proc.time()
    RCodeTimer
}
stopRCodeTimer  <- function(start=NULL) {
    end <- proc.time()
    if(is.null(start)) start <- RCodeTimer
    print(end - start)
}

# find a variable's environment; use in developer tools to validate a variable's scope
getVariableScope <- function(varName){
    isGlobal  <- varName %in% ls(.GlobalEnv)
    isSession <- varName %in% ls(sessionEnv)
    paste(varName,
        if(isGlobal && isSession) "was found in both .GlobalEnv and sessionEnv"
        else if(isGlobal)         "is in .GlobalEnv"
        else if(isSession)        "is in sessionEnv"
        else                      "was not found in .GlobalEnv or sessionEnv"
    )
}

    ## when developing, always recreate the minified framework script
    #if(TRUE){
    #    message("creating minified framework script")
    #    scripts <- list.files(serverEnv$SHARED_DIR, '\\.R$', full.names=TRUE, recursive=TRUE)
    #    text <- unname(unlist(sapply(scripts, function(script){
    #        message(script)
    #        if(any(endsWith(script, c('/global.R','/ui.R','/server.R')))) ""
    #        else if(grepl("INLINE_ONLY", script)) "" # scripts sourced only as needed by client action
    #        else strsplit(loadResourceText(script), "\\n")
    #    })))
    #    text <- gsub("\\r", "", text)
    #    text <- unname(unlist(strsplit(as.character(parse(text = text)), "\n")))
    #    cat( text, sep="\n", file="./TEST.X" )
    #}

