## Analysis Types

Analysis types define a way of analyzing data that can be:

- executed as an synchronous or asynchronous jobs
- potentially reused by many apps

Here's how to set the parameters and handling functions for
use by runAnalyses and potentially other modules.

All analysisTypes must provide, in folder **.../analysisTypes/\<typeGroup\>/\<analysisType\>**

- a **config.yml** file, with members (all are optional):
    - **name** - display name for the analysisType; default = \<analysisType\>
    - **jobType** - how/where the job should be executed; see job_execution.R; default = promise
    - **options** - job execution options, analogous to module settings; see existing prototypes. There are four UI columns for options display; use 'type: empty' for a blank position
    - **packages** - R or Bioconductor packages used by the analysisType; installed but not attached to running server process
    - **classes** - optional data classes used by the analysisType
    - **modules** - optional ui modules used by the analysisType  
  
- **\<analysisType\>_methods.R** file, with the following S3 generic functions:
    - **setJobParameters.\<analysisType\>** - convert reactives to static values for job promises
    - **executeJob.\<analysisType\>** - do the actual work of the job, potentially within a promise
    - **load.\<analysisType\>** - load the output of executeJob() for use by viewer modules

- a **results viewer Shiny module** used to populate the 'results' uiOutput in module viewResults via files:
    - **\<analysisType\>_ui.R**
    - **\<analysisType\>_server.R**
