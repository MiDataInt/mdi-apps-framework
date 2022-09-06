---
title: sourceFileUpload
parent: Standard Step Modules
grand_parent: App Steps
has_children: false
nav_order: 10
---

## {{page.title}} appStep module

The **sourceFileUpload** appStep is the first step module of most apps.
It allows users to:

- add additional data packages to create an extended Analysis Set
- provide a helpful name for the Analysis Set
- provide human-readable names for any Samples in manifestFiles
- examine high-level summaries of the data in their packages

{% include figure.html file="app-steps/source-file-upload.png" border=true %}

For more detailed information, see:

- [mdi-apps-framework : sourceFileUpload](https://github.com/MiDataInt/mdi-apps-framework/tree/main/shiny/shared/session/modules/appSteps/sourceFileUpload)

### Naming sourceFileUpload steps

The canonical name given to sourceFileUpload app steps is 'upload',
which matches the step type declared in _sourceFileUpload/module.yml_.
We recommend following this convention as it promotes consistency and readability.

```yml
# <app>/config.yml
appSteps:
    upload:
        module: sourceFileUpload
```

### Definition and structure of data sources

Each data package loaded by a user is known as a 
**data source**, identified by a unique string identifier derived from 
a hash of the package file. A data source is also known as a **Project**
for purposes of unambiguous sample naming.

For many apps built on data packages with manifestFile declarations,
each data source will carry multiple **samples**, each identified 
by its own unique string identifier. Your app must know how to deal
with an incoming manifestFile by properly handling its declared manifestType,
as described in detail here:

- [mdi-suite-template : manifestTypes](https://wilsonte-umich.github.io/mdi-suite-template/shiny/shared/session/types/manifestTypes/README.html)

### Module options

The sourceFileUpload appStep does not take any non-standard module options.

### Returned outcomes

These are the outcomes returned by the sourceFileUpload module that you may access
in downstream appStep modules.

```r
# sourceFileUpload_server.R
list(
    outcomes = list(
        analysisSetName = reactive(input$analysisSetName),
        sources         = reactive(sources$list),
        samples         = reactive(samples$list), # actually a data.frame
        sampleNames     = reactive(samples$names)        
    )
)
```

where:

- **analysisSetName** = the name the user gave to their analysis set
- **sources** = metadata on the data packages
- **samples** = metadata on the aggregated samples from all packages
- **sampleNames** = any human-readable sample name overrides entered by the user

Typically, apps do not access those outcomes directly, favoring
instead to use the following support utilities.

### Support utilities

These are the utility functions provided by the sourceFileUpload module 
to make it easier to get information about a specific data source or sample.

```r
# <scriptName>.R

# get one or multiple sample names, with user overrides
# arguments allow different means of filtering and sample matching
names <- getSampleNames(rows = TRUE, sampleIds = NULL, sampleUniqueIds = NULL, makeUnique = FALSE)
name  <- getSampleName(sample) # sample is a one row of the samples() table

# get the unique identifiers of samples
uids <- getSampleUniqueIds(samples = NULL, rows = TRUE, sourceId = NULL)

# get a full source, i.e., data package, metadata entry from its ID
source <- getSourceFromId(sourceId)

# get a packaged file from a data source by the file's type or name
dataFileName <- getSourceFile(source, fileType)
dataFilePath <- getSourceFilePath(sourceId, fileType, parentDir = NULL) # when we know a file by type
dataFilePath <- expandSourceFilePath(sourceId, fileName, parentDir=NULL) # when we know a file by name

# get the project name of a data source by ID
projectName <- getSourceFilePackageName(sourceId)

# get option values that were in force during pipeline execution by source ID
# optionFamily and option names are as defined in pipeline.yml
optionValue <- getSourcePackageOption(sourceId, optionFamily, option)
```

For more detailed information, see:

[mdi-apps-framework : sourceFileUpload_utilities](https://github.com/MiDataInt/mdi-apps-framework/blob/main/shiny/shared/session/modules/appSteps/sourceFileUpload/sourceFileUpload_utilities.R)
