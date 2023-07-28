#----------------------------------------------------------------------
# data frame tools
#----------------------------------------------------------------------

# resolve inconsistencies in data frame column names to restore to an expected file format
# function restores one column name per call based on a set of suggested likely alternatives
fixColumnNames <- function(currentNames, correctName, altNames){
    for(altName in altNames) currentNames[currentNames == altName] <- correctName
    currentNames
}

# resolve data type inconsistencies, e.g. due to commas in numbers in a read.table
fixColumnDataTypes <- function(x, template){ # template is an (empty) data frame with the expected column types
    for(col in names(template)){
        if(is.null(x[[col]])) x[[col]] <- 0 # handle missing columns
        inType  <- typeof(x[[col]])
        outType <- typeof(template[[col]])
        if(inType != outType){ # handle data type mismatches
            if(inType == 'character') x[[col]] <- gsub(',', '', x[[col]])
            x[[col]] <- switch(outType,
                'integer' = as.integer(x[[col]]),
                'numeric' = as.numeric(x[[col]]),
                            as.character(x[[col]])
            )     
        }
    }
    x
}

# reduce a data frame to unique rows based on queried columns
uniqueRows <- function(df, cols) df[!duplicated(df[cols]), ] 

#----------------------------------------------------------------------
# vector functions
#----------------------------------------------------------------------
collapseVector <- function(v, n) { # sum every n adjacent elements of a vector
    cv <- unname(tapply(v, (seq_along(v) - 1) %/% n, sum))
    tailLength <- length(v) %% n # number of input elements summed into incomplete last output element    
    if(tailLength != 0){
        cvLength <- length(cv) # expand incomplete last element to same scale as all others
        cv[cvLength] <- cv[cvLength] * n / tailLength          
    }
    cv
}
uncollapseVector <- function(v, n, len) { # reverse the actions of collapseVector
    ucv <- as.vector(sapply(v, rep, n))
    extra <- length(ucv) - len # user must remember how long the original vector was    
    if(extra > 0) ucv <- ucv[1:len]    
    ucv
}
expandVector <- function(v, n){
    as.vector(sapply(v, rep, n))
}

#----------------------------------------------------------------
# miscellaneous functions
#----------------------------------------------------------------

# shortcut for the opposite of %in%
`%notin%` <- Negate(`%in%`)

# logical 'or' of is.null and is.na; not usually preferred but sometimes convenient
is.nonexistent <- function(x) is.null(x) | is.na(x)

# determine which elements of a vector are, or can be converted, to numeric values
check.numeric <- function(x) !is.na(suppressWarnings(as.numeric(x)))

# extend shiny:isTruthy() to determine whether an object has data
# thus, zero-length vectors and lists and data.frames without no rows return FALSE
objectHasData <- function(x){
    isTruthy(x) && {
        if(is.vector(x)) length(x) > 0 # handles vectors and lists
        else if(is.data.frame(x)) nrow(x) > 0 # handles data.frame, data.table, etc.
        else TRUE
    }
}

#----------------------------------------------------------------
# bit64 helpers
#----------------------------------------------------------------
lapply64 <- function (X, FUN, ...) { # make sure lapply and sapply retain the integer64 class
    FUN <- match.fun(FUN)
    if (!is.vector(X) || is.object(X)){
        X <- as.list(X)
        class(X) <- "integer64" # <<- this line was added relative to lapply 
    }
    .Internal(lapply(X, FUN))
}
sapply64 <- function (X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) {
    FUN <- match.fun(FUN)
    answer <- lapply64(X = X, FUN = FUN, ...) # <<- this line was modified relative to sapply
    if (USE.NAMES && is.character(X) && is.null(names(answer))) 
        names(answer) <- X
    if (!isFALSE(simplify) && length(answer)) 
        simplify2array(answer, higher = (simplify == "array"))
    else answer
}

#----------------------------------------------------------------------
# shared resource tools
#----------------------------------------------------------------------

## get the path to a shared resource file, served by switchboard
#sharedResource <- function(file) paste(serverEnv$SHARED_RESOURCE_URL, file, sep="/")

## load the full text of a resource file stored in the app container
## NB: does not work for compressed files!
#loadResourceText <- function(fileName) readChar(fileName, file.info(fileName)$size)
