---
title: dataSourceTable
parent: Data Selectors
has_children: false
nav_order: 10
---

## {{page.title}}

The **dataSourceTable** widget creates a table that
allows users to select one or more data sources, i.e.,
uploaded data packages. 

{% include figure.html file="data-selectors/data-source.png" border=true %}

It is most useful for apps 
that do not have samples, or
if your app can assume (and verify) a specific structure for the 
samples in a given data package.

The table is wrapped in a shinydashboard `box()`.

### dataSourceTableUI options

The `dataSourceTableUI` function takes the following arguments in addition to 'id':

```r
# dataSourceTable_ui.R
dataSourceTableUI(
    id, 
    title, 
    width = 12, 
    collapsible = FALSE
)
```

where:

- **title** = the title for the box
- **width** = the width of the box in bootstrap grid units
- **collapsible** = whether to allow the user to hide/close the box contents

### dataSourceTableServer options

The `dataSourceTableServer` function takes the following arguments in addition to 'id':

```r
# dataSourceTable_server.R
dataSourceTableServer <- function(
    id, 
    selection = "single"
) 
```

where:

- **selection** = either 'single' or 'multiple' to determine how many data sources can be selected at a time

### dataSourceTableServer return values

The module returns a reactive as follows:

```r
# dataSourceTable_server.R
selectedSourceIds
```

where _selectedSourceIds_ is a reactive that returns `names(sources()[rows])`, i.e.,
the unique ids of the selected subset of the `sources` outcome returned by appStep module `sourceFileUpload`. Those sourceId(s) can be used to recover files from data packages, etc.

### Using the widget

`dataSourceTable` is a simple widget. Just place it on a page
by calling `dataSourceTableUI` in your module UI function, activate it by
calling `dataSourceTableServer` in your module server function, and react
to the `selectedSourceIds` return value.

### Additional references
 
For more detailed views of the module's code, see:

- [mdi-apps-framework : dataSourceTable](https://github.com/MiDataInt/mdi-apps-framework/tree/main/shiny/shared/session/modules/widgets/tables/dataSourceTable)
