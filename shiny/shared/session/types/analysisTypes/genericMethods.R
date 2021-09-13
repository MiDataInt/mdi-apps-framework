
#----------------------------------------------------------------------
# define generic functions, i.e. S3 methods, applied to analysisTypes
#----------------------------------------------------------------------

# setJobParameters generates the jobParameters object sent to executeJob
# a main purpose is to convert reactives to static variables for use by promises
setJobParameters <- function (x, ...) {
    UseMethod("setJobParameters", x)
}

# executeJob does the actual analysis work
#   called by runJobXXX family of functions, often within a promise
#   if jobType is not 'immediate', must not depend on any reactives
executeJob <- function (x, ...) {
    UseMethod("executeJob", x)
}

# load the results of an analysis for use in viewResults or related ui modules
loadJobOutput <- function (x, ...) {
    UseMethod("loadJobOutput", x)
}

