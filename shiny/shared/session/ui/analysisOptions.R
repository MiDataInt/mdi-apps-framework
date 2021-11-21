#----------------------------------------------------------------------
# construct and access the option inputs on the runAnalyses module form UI
#----------------------------------------------------------------------

# parse the choices to fill into an analysis options selectInput
getAnalysisOptionChoices <- function(analysisTypeNames, t){
    x <- if(is.null(t$choices)) analysisTypeNames # special handling of analysisTypes selectInput
    else if(is.character(t$choices) &&
            length(t$choices) == 1 &&
            exists(t$choices)) get(t$choices)() # look up choices from a function or reactive
    else t$choices # choices hard coded into analysisType config.yml
    if(!is.null(t$startWithNull) && t$startWithNull) x <- c(CONSTANTS$nullSelectSetOption, x)
    x
}

# return options as form inputs
getAnalysisOptionInputs <- function(ns, analysisTypeNames, analysisOption, value=NULL){
    stepName <- appStepNamesByType$analyze
    optionName <- names(analysisOption)
    label <- gsub('_', ' ', optionName)
    id <- ns(optionName)    
    t <- analysisOption[[1]] # t = template
    if(is.null(value)) value <- t$value # apply defaults
    getOption <- function(name, default=NA) if(is.null(t[[name]])) default else t[[name]]
    div(switch(
        t$type,
        empty = "", # honor request for blank positions in 4-columned rows of inputs
        selectInput = selectInput(id, label, 
                                  choices = getAnalysisOptionChoices(analysisTypeNames, t), 
                                  selected = value),
        numericInput = numericInput(id, label, value, getOption('min'), getOption('max'), getOption('step')),
        get(t$type)(id, label, value)
    ), style = "margin-bottom: 5px;")
}

# get the currently selected option values from the edit panel
# return a list of values (not friendly display names)
getOptionValuesFromUI <- function(analysisOptions, input, addOptions=NULL){
    analysisOptions <- parseOptionValuesRequest(analysisOptions, addOptions)
    values <- list()    
    if(length(analysisOptions) == 0) return(values)
    optionNames <- names(analysisOptions)
    lapply(optionNames, function(optionName) input[[optionName]]) %>% setNames(optionNames)
}
parseOptionValuesRequest <- function(analysisOptions, addOptions){
    if(is.null(analysisOptions)) analysisOptions <- list()    
    if(!is.null(addOptions)) analysisOptions <- c(addOptions, analysisOptions)  
    analysisOptions    
}

# get the option values stored in a saved analysis schema
# return values as a vector of the same length as c(addOptions, analysisOptions)
getOptionValuesFromSchema <- function(analysisOptions, schema, addOptions=NULL){
    if(is.null(schema)) return(NULL)
    analysisOptions <- parseOptionValuesRequest(analysisOptions, addOptions)
    if(length(analysisOptions) == 0) return(NULL)
    sapply(names(analysisOptions), function(optionName) schema[[optionName]])
}

# get the option values stored in a saved analysis schema
# return in a compact form suitable for display in a UI table row, etc.
getOptionsUIFromSchema <- function(analysisTypeNames, analysisOptions, schema, addOptions=NULL, exclude=character()){
    if(is.null(schema)) return('')
    stepName <- appStepNamesByType$analyze
    analysisOptions <- parseOptionValuesRequest(analysisOptions, addOptions)    
    if(length(analysisOptions) == 0) return('')        
    optionNames <- names(analysisOptions)
    schemaNames <- names(schema)
    schema <- schema[schemaNames %in% optionNames & schemaNames %notin% exclude] # a named list of values
    optionNames <- names(schema)
    optionValues <- sapply(optionNames, function(optionName){
        t <- analysisOptions[[optionName]]
        if(t$type == 'selectInput') {
            choices <- getAnalysisOptionChoices(analysisTypeNames, t)
            names(choices)[choices == schema[[optionName]]]
        }
        else schema[[optionName]]
    })
    paste(mapply(paste, optionNames, '=', optionValues), collapse = "<br>")
}
