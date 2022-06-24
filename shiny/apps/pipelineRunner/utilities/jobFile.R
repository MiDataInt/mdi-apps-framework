#----------------------------------------------------------------------
# convert input option values to job yml (for writing) and vice versa (for loading)
#----------------------------------------------------------------------


#----------------------------------------------------------------------
# use 'mdi <pipeline> optionsTable' to recover comprehensive information about options
# only developer's default values are present in the table, along with all option metadata
#----------------------------------------------------------------------

getPipelineOptionsTable <- function(pipeline){
    args <- c(pipeline, 'optionsTable')
    optionsTable <- runMdiCommand(args)
    req(optionsTable$success)
    optionsTable <- fread(text = optionsTable$results)
    optionsTable$required <- as.logical(optionsTable$required)
    optionsTable 
}

#----------------------------------------------------------------------
# use 'mdi <pipeline> template --all-options' to determine the default values for a pipeline, 
# _without_ considering a folder environment, returned as parsed job yml
# thus, these values are determined purely by the developer's pipeline.yml
#----------------------------------------------------------------------
getPipelineTemplate <- function(pipeline){
    args <- c(pipeline, 'template', "--all-options") 
    template <- runMdiCommand(args)
    req(template$success)
    read_yaml(text = template$results)
}

#----------------------------------------------------------------------
# determine the default values specific to a folder environment
# using a minimal, temporary <data>.yml with no values specified
#----------------------------------------------------------------------
getJobEnvironmentDefaults <- function(dir, pipeline, actions = NULL){ # dir is the final intended location of a job file
    if(is.null(actions)) actions <- getPipelineTemplate(pipeline)$execute
    nullYml <- list(
        pipeline = pipeline,
        execute = actions
    )
    nullFile <- file.path(dir, "_NULL_.yml") 
    write_yaml(nullYml, file = nullFile)
    defaults <- readDataYml(list(path = nullFile))
    unlink(nullFile)    
    defaults
}

#----------------------------------------------------------------------
# use 'mdi <pipeline> valuesYaml' to recover the context-dependent job values from <data>.yml
#----------------------------------------------------------------------
# values arrive in precedence order:
#    <data>.yml              (highest precedence)
#    <pipeline>.yml          (i.e., user's local folder overrides)
#    stage1-pipelines.yml    (i.e., MDI installation defaults)
#    pipeline default        (lowest precedence)
#----------------------------------------------------------------------
readDataYml <- function(jobFile){
    args <- c('valuesYaml', 'valuesYaml', jobFile$path)
    valuesYaml <- runMdiCommand(args, suite = jobFile$suite, errorDialog = function(...) showUserDialog(
        "Job File Load Error", 
        tags$p("A fatal error occurred while attempting to load the following job file:"), 
        tags$p(jobFile$path, style = "margin-left: 2em;"),
        tags$p("Most likely there is a configuration error in the job file."), 
        tags$p("See the server log output for additional debugging information."), 
        size = "m", 
        type = 'okOnly'
    ))
    req(valuesYaml$success)
    read_yaml(text = valuesYaml$results) # retains suite//option name format
}

#----------------------------------------------------------------------
# construct and write a yaml file with all informative options specified
#----------------------------------------------------------------------
# informative options are those that are different from:
#       <pipeline>.yml
#       stage1-pipelines.yml
#       the pipeline default
#----------------------------------------------------------------------
# Thus, if future changes are made to <pipeline>.yml or stage1-pipelines.yml,
# they will be implicitly propagated into <data>.yml. This mirrors the 
# behavior expected of manual file editing, where the intent is that values
# specified in <data>.yml are only those that define a specific job instance,
# not those that reflect properties assigned by the job's environment.
#----------------------------------------------------------------------
writeDataYml <- function(jobFilePath, suite, pipeline, newValues, 
                         actions = NULL, optionsTable = NULL, template = NULL,
                         allOptions = FALSE){

    # initialize yaml
    if(is.null(optionsTable)) optionsTable <- getPipelineOptionsTable(pipeline)
    if(is.null(template)) template <- getPipelineTemplate(pipeline)
    allActions <- template$execute    
    if(is.null(actions)) actions <- allActions
    yml <- list(pipeline = file.path(suite, pipeline))

    # helper objects for parsing values
    defaults <- getJobEnvironmentDefaults(dirname(jobFilePath), pipeline, allActions)
    option_ <- NULL
    typeFn  <- NULL
    REQUIRED <- "_REQUIRED_"
    adjustValue <- function(value){
        if(option_$required){
            if(is.null(value) || value == "" || value == REQUIRED) REQUIRED else typeFn(value)
        } else {
            if(is.null(value) || value == "") NULL else typeFn(value)
        }
    }

    # build the nested action options, requested options only
    for(actionName in allActions){ # print options for all actions, even if not executed to protect future interests
        action_ <- template[[actionName]]
        for(family in names(action_)){
            family_ <- action_[[family]]
            for(option in names(family_)){

                # collect information on option attributes and value states
                option_ <- optionsTable[action == actionName & 
                                        optionFamily == family &
                                        optionName == option]
                default  <-  defaults[[actionName]][[family]][[option]] # environment-specific default
                newValue <- newValues[[actionName]][[family]][[option]]

                # adjust the values to account for required flag and type
                typeFn <-      if(option_$type == "boolean") as.logical
                          else if(option_$type == "integer") as.integer
                          else if(option_$type == "double")  as.double
                          else as.character
                default  <- adjustValue(default)
                newValue <- adjustValue(newValue)

                # commit informative values per rules listed above
                if(allOptions || 
                   !identical(default, newValue) || # the value was not the one provided by the environment
                   (option_$required && newValue == REQUIRED)){ # a missing value is required (and was not provided by the environment) # nolint
                    if(is.null(yml[[actionName]])) yml[[actionName]] <- list()
                    if(is.null(yml[[actionName]][[family]])) yml[[actionName]][[family]] <- list()
                    yml[[actionName]][[family]][[option]] <- newValue
                }
            } 
        }
    }

    # build and write the final yaml in proper element order
    yml$execute <- actions
    write_yaml(yml, file = jobFilePath)
}
