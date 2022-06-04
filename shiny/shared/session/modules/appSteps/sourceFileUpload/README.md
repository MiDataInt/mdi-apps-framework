---
title: sourceFileUpload
parent: Standard Step Modules
grand_parent: App Steps
has_children: false
nav_order: 10
---

## {{page.title}} appStep module

The **sourceFileUpload** appStep is the first module of most apps.
It allows users to:

- add additional data packages to create an extended analysis set
- provide a helpful name for the analysis set
- provide human-readable names for any samples in manifestFiles
- examine high-level summaries of the data in their packages

For more detailed information, see:

- [mdi-apps-framework : sourceFileUpload](https://github.com/MiDataInt/mdi-apps-framework/tree/main/shiny/shared/session/modules/appSteps/sourceFileUpload)

## Definition and structure of data sources

Each data package loaded by a user is known as a 
**data source**, identified by a unique string identifier derived from 
a hash of the package file. A data source is also known as a **Project**
for purposes of unambiguous sample identification.

For many apps built on data packages with manifestFile declarations,
each data source will carry multiple **samples**, each identified 
by its own unique string identifier. Your app must know how to deal
with the incoming manifestFile by properly handling the declared manifestType,
as described in detail here:

- [mdi-suite-template : manifestTypes](https://wilsonte-umich.github.io/mdi-suite-template/shiny/shared/session/types/manifestTypes/README.html)

## Returned outcomes

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

- **analysisSetName** = the name the user typed in for their analysis set
- **sources** = metadata on the data packages
- **samples** = metadata on the aggregated samples from all packages
- **sampleNames** = any human-readable sample name overrides entered by the user

## Support utilities

These are the utility functions provided by the module to make it
easier to get information about a specific data source or sample,
in addition to examining the module outcomes directly.

```r
# get one or multiple same names, with user overrides
getSampleNames <- function(rows = TRUE, sampleIds = NULL, sampleUniqueIds = NULL, makeUnique = FALSE)
getSampleName <- function(sample){ # sample is a one row of the samples() table

# get the unique identifiers of samples
getSampleUniqueIds <- function(samples=NULL, rows=TRUE, sourceId=NULL)

# get the full source entry from its ID
getSourceFromId <- function(sourceId)

# get a file from a source, i.e. data package, by type or name
getSourceFile <- function(source, fileType) # just the file name
getSourceFilePath <- function(sourceId, fileType, parentDir=NULL) # when we know a file by type
expandSourceFilePath <- function(sourceId, fileName, parentDir=NULL) # when we know a file by name
getSourceFilePackageName <- function(sourceId)

# get information about a data package from its source ID
# optionFamily and option names are the values in force during pipeline execution
getSourcePackageOption <- function(sourceId, optionFamily, option)
```

For more detailed information, see:

[mdi-apps-framework : sourceFileUpload_utilities](https://github.com/MiDataInt/mdi-apps-framework/blob/main/shiny/shared/session/modules/appSteps/sourceFileUpload/sourceFileUpload_utilities.R)
