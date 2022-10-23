---
title: selectSamples
parent: Standard Step Modules
grand_parent: App Steps
has_children: false
nav_order: 20
---

## {{page.title}} appStep module

The **selectSamples** appStep is a first step module that is an
alternative to sourceFileUpload for apps that do not work from
pipeline data packages, i.e., that instead work as cold-starting apps
that load sample data from other types of files or databases.

Specifically, selectSamples allows developers and users to:

- list a large collection of Samples for users to choose from
- select a subset of those Samples for use in an Analysis Set
- provide a helpful name for the Analysis Set
- provide alternative human-readable names for Samples

For more detailed information, see:

- [mdi-apps-framework : selectSamples](https://github.com/MiDataInt/mdi-apps-framework/tree/main/shiny/shared/session/modules/appSteps/selectSamples)

### Naming selectSamples steps

The canonical name given to selectSamples app steps is 'select',
which matches the step type declared in _selectSamples/module.yml_.
We recommend following this convention as it promotes consistency and readability.

```yml
# <app>/config.yml
appSteps:
    select:
        module: selectSamples
```

For consistency with sourceFileUpload, the selectSamples module configuration
file also assigns type `upload` to the first app step.

### Definition and structure of data sources

Because selectSamples assumes that your app's users will draw a subset of samples
from some externally defined universe of samples, it has no built-in
concept of data sources. It simply assigns the same data source,
called `allSamples`, to every sample in an analysis set. This is an invisible, internal assignment done for consistency with sourceFileupload, 
you should not normally need to worry about it.

### Module options

The selectSamples appStep takes the following non-standard module options.
The listing below shows the defaults values, which you can override in your
app's _config.yml_ file as needed. 

```yml
# appSteps/selectSamples/module.yml
selectedWidth:       4
availableWidth:      8
cacheAvailableSamples: cacheAvailableSamples
sampleIdCol:           Sample_ID
descriptionCol:        Description
selectedSamplesTemplate: selectedSamplesTemplate
availableSamplesTemplate: availableSamplesTemplate
```

where:

- **selectedWidth** = width of the selected samples box, in bootstrap grid units
- **availableWidth** = width of the available samples box, in bootstrap grid units
- **cacheAvailableSamples** = the name of a function that saves a data.frame of all available samples to argument `cacheFile` using `saveRDS()`
- **sampleIdCol** = the column in the data.frame written by `cacheAvailableSamples` that carries a unique sample identifier
- **descriptionCol** = the column in the data.frame written by `cacheAvailableSamples` that carries a text description of the sample
- **selectedSamplesTemplate** = the name of a (typically empty) data.frame with the columns to show in the selected samples table
- **availableSamplesTemplate** = the name of a (typically empty) data.frame with the columns to show in the available samples table

The `cacheAvailableSamples(cacheFile)` function and 
`selectedSamplesTemplate` and `availableSamplesTemplate` data.frames
are typically defined at the top of your app's _server.R_ file. 

```r
# apps/<app>/server.R
cacheAvailableSamples <- function(cacheFile){
    ...
    unlink(cacheFile)
    saveRDS(..., file = cacheFile)
}
selectedSamplesTemplate <- data.table(
    XXXX = character()
)
availableSamplesTemplate <- data.table(
    XXXX = character()
    YYYY = integer()
)
```

Notice that the function and data.frame options are simple string values
that are retrieved by name using the R `get()` function.

### Returned outcomes

The outcomes returned by selectSamples have the exact same structure
as sourceFileUpload, for consistency of use of downstream appStep
modules such as assignSamples.

### Support utilities

Relevant utility functions provided by the sourceFileUpload module 
will work equivalently for selectSamples.
