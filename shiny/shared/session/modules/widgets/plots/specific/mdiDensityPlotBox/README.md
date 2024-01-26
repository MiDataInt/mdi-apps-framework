---
title: mdiDensityPlotBox
parent: Specific Plots
grand_parent: Display Widgets
has_children: false
nav_order: 10
---

## {{page.title}}

The **mdiDensityPlotBox** widget creates a display box
that supports plotting one or more superimposed data 
density distributions in various ways.

Understanding and comparing the value distributions of data series
is a fundamental analysis task that
can reveal many things about your data. These include
the nature of a single distribution (normal, skewed, etc.)
and how two data groups differ.

### mdiDensityPlotBox options

The `mdiDensityPlotBox` function takes the following arguments in addition to 'id':

```r
# staticPlotBox_ui.R
mdiDensityPlotBoxUI <- function(id, title, ...)
```

where:

- **title** = the title of the plot box

### mdiDensityPlotBoxServer options

The `mdiDensityPlotBoxServer` function takes the following arguments in addition to 'id':

```r
# staticPlotBox_server.R
mdiDensityPlotBoxServer <- function(
    id,
    data,
    groupingCols,
    xlab,
    defaultBinSize = 1,
    eventTypePlural = "Events",
    ... 
)
```

where:

- **data** =  a data.table with columns x and groupingCols, or a reactive that returns one
- **groupingCols** =  column(s) that define the groups to summarize as distinct distributions, or a reactive that returns one; can be NULL
- **xlab** =  x axis label, or a reactive that returns one 
- **defaultBinSize** =  starting bin resolution on the X axis, subject to user override
- **eventTypePlural** = name of the thing being counted for the plot title
- **...** = additional options passed to mdiXYPlot()

### mdiDensityPlotBoxServer return value

The module returns, as is, the value from `staticPlotBoxServer()` for 
the staticPlotBox it generates.

### Supplying data to the widget

The data.table provided to `mdiDensityPlotBox` must have an `x` columns, plus any
columns provided as `groupingCols` that are used to define data groups. The widget
will parse the data into left-justified bins on the X axis and determine either the count
or frequency of events per group per X axis bin. The method ensures that all 
bins are represented for all groups, with zero values as needed, to ensure proper plotting.

### Using the widget

First, place an instance of the mdiDensityPlotBox widget in your UI 
(only widget-related code is shown):

```r
# <scriptName>_ui.R
mdiDensityPlotBoxUI(
    ns('id'),
    title = "My Title",
    # ...
)
```

Then activate the plot in the matching server:

```r
# <scriptName>_server.R
myPlot <- mdiDensityPlotBoxServer(
    id = "id",
    data = myDataReactive,
    groupingCols = myColsReactive, # or a fixed set of column names
    xlab = "My X Label",
    defaultBinSize = 1,
    x0Line = TRUE # etc.
)
```

### Additional references
 
For more detailed views of the module's code, see:

- [mdi-apps-framework : mdiDensityPlotBox](https://github.com/MiDataInt/mdi-apps-framework/blob/main/shiny/shared/session/modules/widgets/plots/specific/mdiDensityPlotBox)
