#----------------------------------------------------------------
# app code development tools
#----------------------------------------------------------------

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
