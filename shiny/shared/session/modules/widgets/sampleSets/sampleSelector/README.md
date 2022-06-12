---
title: sampleSelector
parent: Data Selectors
has_children: false
nav_order: 30
---

## {{page.title}}

The **sampleSelector** widget provides a way of selecting
samples that builds on the grid-based architecture of
the `assignSamples` module. A first input allows users
to select a Sample Set. A button then allows them
to open a modal popup with a grid of checkboxes to
select all or just a subset of the samples in the set.

{% include figure.html file="data-selectors/sample-selector.png" border=true %}

The dual approach give users maximum flexibility in
constructing a specific analysis. An overall experimental
design can be laid out with `assignSamples` and 
analysis subsets can be established using `samplesSelector`.

### sampleSelectorUI options

The `sampleSelectorUI` function does not take any arguments other than 'id'.

```r
# sampleSelector_ui.R
sampleSelectorUI <- function(id)
```

### sampleSelectorServer options

The `sampleSelectorServer` function takes the following arguments in addition to 'id':

```r
# sampleSelector_server.R
sampleSelectorServer <- function(
    id,
    parentId
)
```

where:

- **id** = the id of the table widget
- **parentId** = the id of the module loading the widget

### sampleSelectorServer return values

The module returns a list as follows (some methods primarily for internal
use are not listed here):

```r
# sampleSelector_server.R
list(
    allAssignments      = allAssignments,
    selectedAssignments = selectedAssignments,
    allSamples          = allSamples,  
    sampleSet           = reactive({ input$sampleSet }),
    selectedSamples = reactive(...),
    input = input
)
```

where:

- **allAssignments** = a reactive that returns the data.frame of all assignments for the selected sample set, i.e., not reflecting user selections
- **selectedAssignments** = a reactive that returns the subset of `allAssignments` rows matching the selected samples
- **allSamples** = a reactive that returns a vector of the uniqueId(s) of all samples in the selected sample set
- **sampleSet** = a reactive that returns the unique ID of the selected sample set
- **selectedSamples** = a reactive that returns the subset of `allSamples` for the selected samples
- **input** = the input object for the widget

where `assignments` corresponds to the outcome of the same name from the `assignSamples`
appStep module and a sample uniqueId is of the form 'Project:Sample_ID'.

### Using the widget

`sampleSelector` is a simple widget. Just place it on a page
by calling `sampleSelectorUI` in your module UI function, activate it by
calling `sampleSelectorServer` in your module server function, and react
to appropriate reactive(s) in its return value list for your app's needs.

### Additional references
 
For more detailed views of the module's code, see:

- [mdi-apps-framework : sampleSelector](https://github.com/MiDataInt/mdi-apps-framework/tree/main/shiny/shared/session/modules/widgets/sampleSets/sampleSelector)
