
here's how to set the parameters and handling functions for analysis jobs
used by the runAnalyses and potentially other modules

all analysisTypes must provide, in folder shiny/shared/optional/types/analysisTypes/<typeGroup>/<type>

a config.yml file with members (all are optional):
    name: display name for the analysisType; default = <analysisType>
    jobType: how/where the job should be executed; see job_execution.R; default = promise
    options: job execution options, analogous to module settings; see existing prototypes
             there are four columns for options display; use 'type: empty' for a blank position
    packages: R or Bioconductor packages used by the analysisType; installed but not attached to running Portal
    classes: optional data classes used by the analysisType
    modules: optional ui modules used by the analysisType

<analysisType>_methods.R file with the following S3 generic functions:
    setJobParameters.<analysisType> = convert reactives to static values for job promises,
    executeJob.<analysisType> = do the actual work of the job, potentially within a promise
    load.<analysisType> = load the output of executeJob for use by viewer modules

a results viewer module used to populate the 'results' uiOutput in module viewResults via files:
    <analysisType>_ui.R
    <analysisType>_server.R

