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

#----------------------------------------------------------------
# miscellaneous functions
#----------------------------------------------------------------

# shortcut for the opposite of %in%
`%notin%` <- Negate(`%in%`)

# logical 'or' of is.null and is.na; not usually preferred but sometimes convenient
is.nonexistent <- function(x) is.null(x) | is.na(x)

#----------------------------------------------------------------------
# shared resource tools
#----------------------------------------------------------------------

## get the path to a shared resource file, served by switchboard
#sharedResource <- function(file) paste(serverEnv$SHARED_RESOURCE_URL, file, sep="/")

## load the full text of a resource file stored in the app container
## NB: does not work for compressed files!
#loadResourceText <- function(fileName) readChar(fileName, file.info(fileName)$size)
