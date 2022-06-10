---
title: sampleSet
parent: Data Selectors
has_children: false
nav_order: 20
---

## {{page.title}}

The **sampleSet** widget provides a single input to select
a sample set and returns the samples assigned to that set.

### sampleSetUI options

The `sampleSetUI` function does not take any arguments other than 'id'.

```r
# sampleSet_ui.R
sampleSetUI <- function(id)
```

### sampleSetServer options

The `sampleSetServer` function takes the following arguments in addition to 'id':

```r
# sampleSet_server.R
sampleSetServer <- function(
    id,
    parentId
)
```

where:

- **id** = the id of the table widget
- **parentId** = the id of the module loading the widget

### sampleSetServer return values

The module returns a list as follows:

```r
# sampleSet_server.R
list(
    assignments = assignments,
    input = input
)
```

where:

- **assignments** = a reactive that returns a data.frame of the samples assigned to the selected sampleSet
- **input** = the input object for the widget

where `assignments` corresponds to the outcome of the same name from the `assignSamples` appStep module.

### Using the widget

`sampleSet` is a simple widget. Just place it on a page
by calling `sampleSetUI` in your module UI function, activate it by
calling `sampleSetServer` in your module server function, and react
to the `assignments` element in its return value list.

### Additional references
 
For more detailed views of the module's code, see:

- [mdi-apps-framework : sampleSet](https://github.com/MiDataInt/mdi-apps-framework/tree/main/shiny/shared/session/modules/widgets/sampleSets/sampleSet)
