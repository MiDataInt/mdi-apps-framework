#----------------------------------------------------------------------
# convert input option values to job yml (for writing) and vice versa (for loading)
#----------------------------------------------------------------------

# use 'mdi <pipeline> valuesTable' to recover the context-dependent job values from <data>.yml
readDataYml <- function(jobFile){
    args <- c('valuesTable', 'valuesTable', jobFile$path)
    valuesTable <- runMdiCommand(args)
    req(valuesTable$success)
    read_yaml(text = valuesTable$results)
}

# # parse inputs to a partial <data>.yml file
# parseDataYml <- function(){
#     config <- pipelineConfig()
#     req(config)  

#     # first save, include all actions to start, option values from template
#     if(is.null(jobFile())) return(config$template)

#     # saving after option value changes, requested actions only
#     req(input$actions)

#     # get the option values for each action
#     x <- lapply(c("_PIPELINE_", input$actions), function(actionName){
#         if(actionName == "_PIPELINE_") return( paste(suiteName(), input$pipeline, sep = "/") )
#         options <- config$options[action == actionName]
#         optionFamilyNames <- options[, unique(optionFamily)]
#         x <- lapply(optionFamilyNames, function(optionFamilyName){
#             options <- options[optionFamily == optionFamilyName]
#             x <- lapply(seq_len(nrow(options)), function(i){
#                 option <- options[i]
#                 value <- {
#                     id <- paste('input', option$optionName, sep = "_")
#                     value <- input[[id]]
#                     if(option$type == "boolean") value <- if(is.null(value)) 0 else 1
#                     if(option$required || value != option$default) value else NULL
#                 }
#                 if(!is.null(value) && value == "_NA_") value <- "NA" 
#                 value
#             })
#             names(x) <- options$optionName
#             x
#         })
#         names(x) <- optionFamilyNames
#         x
#     })
#     names(x) <- c("pipeline", input$actions)
#     x$execute <- input$actions
#     x
# }