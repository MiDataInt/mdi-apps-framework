---
title: mdiInteractivePlot
parent: Plots
grand_parent: Display Widgets
has_children: false
nav_order: 30
---

## {{page.title}}

The **mdiInteractivePlot** plot widget displays a plot
that is drawn as a static plot or image, but offers a
set of interactive features implemented using a combination
of Shiny and javascript code. 

### mdiInteractivePlotUI options

The `mdiInteractivePlotUI` function takes the following arguments in addition to 'id':

```r
# mdiInteractivePlot_ui.R
mdiInteractivePlotUI <- function(
    id,  
)
```

where:

- **xx** = xxx

### mdiInteractivePlotServer options

The `mdiInteractivePlotServer` function takes the following arguments in addition to 'id':

```r
# mdiInteractivePlot_server.R
mdiInteractivePlotServer <- function(
    id,
    
)
```

where:

- **xx** = xx

### mdiInteractivePlotServer return values

The module returns a list as follows:

```r
# mdiInteractivePlot_server.R
list(
    xx        = xx,
)
```

where xx.

### Using the widget

First, place an instance of the mdiInteractivePlot widget in your UI 
(only widget-related code is shown):

```r
# <scriptName>_ui.R
mdiInteractivePlotUI(
    ns('id')
    # ...
)
```

Then activate the plot in the matching server and define
the function that will fill (i.e., create) the plot:

```r
# <scriptName>_server.R
myPlot <- mdiInteractivePlotServer(
    'id', 
    xx
)
```

### Additional references
 
For more detailed views of the module's code, see:

- [mdi-apps-framework : mdiInteractivePlot](https://github.com/MiDataInt/mdi-apps-framework/blob/main/shiny/shared/session/modules/widgets/plots/mdiInteractivePlot)

For a complete working example, see:

- [genomex-mdi-tools : trackBrowser](https://github.com/wilsontelab/genomex-mdi-tools)
