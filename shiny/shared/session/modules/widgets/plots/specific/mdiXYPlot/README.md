---
title: mdiXYPlot
parent: Specific Plots
grand_parent: Display Widgets
has_children: false
nav_order: 20
---

## {{page.title}}

The **mdiXYPlot** widget create a an XY plot, also called a scatterplot, 
in a pre-existing staticPlotBox
from an input `data.table`,
where one or more groups of data have values with X and Y coordinates.

The XY plot can be constructed with 
points, lines, both, filled areas, or histograms,
per the calling module's and perhaps the user's settings.
An appropriate color legend defining the groups is added to the right plot margin.

The plot and legend
must be wrapped in a `staticPlotBox`, but `mdiXYPlot` itself
does not provide the box, it provides functions that can
be used to fill the box. This organization can allow users to switch
between different data representations.

### mdiXYPlot settings

To help users set XY plot display attributes, you must access the
`mdiXYPlotSettings` list to obtain a template for
the mdiXYPlot settings. See the example below.

### Calling mdiXYPlot

The `mdiXYPlot` function takes the following arguments:

```r
# mdiXYPlot.R
mdiXYPlot <- function(
    plot,
    dt,
    xlim,
    ylim,
    groupingCols = NULL,
    groupColors = NULL,
    plotAs = c("points","lines","both","area","histogram"),
    legendTitle = "",
    h = NULL,
    v = NULL,
    hColor = "grey60",
    vColor = "grey60",
    x0Line = FALSE,
    y0Line = FALSE,
    histogramSpacing = NULL
)
```

where:

- **plot** = the staticPlotBox we are populating, must include mdiXYPlotSettings
- **dt** = a data.table with columns x, y, and any groupingCols
- **xlim** = the plot X-axis limits
- **ylim** = the plot Y-axis limits
- **groupingCols** = if provided, columns in dt used to define the plotting groups
- **groupColors** = if provided, a named list of colors per group
- **plotAs** = how to render the XY data series
- **legendTitle** = header for the color legend
- **h** = Y-axis values at which to place line rules
- **v** = X-axis values at which to place line rules
- **hColor** = the color(s) used to draw h rules
- **vColor** = the color(s) used to draw v rules
- **x0Line** = add a vertical black line at x = 0
- **y0Line** = add a horizontal black line at x = 0
- **histogramSpacing** = the X-axis distance alotted to histogram bars of the same X value

Calling `mdiXYPlot()` renders the plot by calling `points()`, `lines()`, etc., so your calling code must already
have initialized an empty plot frame, e.g., by calling `plot()` or `staticPlotBox$initializeFrame()`.

### mdiXYPlot types

`mdiXYPlot` supports all the kinds of plots you'd expect for an R plot,
matching the standard plot types, i.e., point, lines, and histograms,
as well as filled area plots.

One color is used to represent each group in the data, as determined from `groupColors`. 
If `groupColors` is NULL, colors are taken in order from `CONSTANTS$plotlyColors`.
Users can add transparency to plot elements using setting `XY_Plot$Color_Alpha`.
A legend is always added to the right plot margin to reveal the group plotting colors;
legend colors are always full opacity.

Group plotting order is determined by user settings `XY_Plot$Group_Order` and `XY_Plot$Reverse_Group_Order`, 
and the plotting of scatterplot points within each group is determined by setting `XY_Plot$Point_Order`. 
Other types are plotted in numerical order on the X axis.
Final settings allow users to add noise, i.e., jitter, to scatterplots. Together,
random scatterplot point order, jittering, and/or color transparency can help visualize dense plots.

### Supplying data to the widget

The data.table provided to `mdiXYPlot` must have `x` and  `y` columns, plus any
columns provided as `groupingCols` that are used to define data groups.
If you would like to help users toward a specific group plotting order,
you should pre-order your groups in the data.table.

### Typical usage

```R
myPlot <- staticPlotBoxServer(
    "myPlot",
    margins = TRUE,
    settings = mdiXYPlotSettings,
    create = function() {
        # reactive code to establish the plot data and limits
        myPlot$initializeFrame(...)
        mdiXYPlot(...)
    }
)
```

### Additional references
 
For more detailed views of the module's code, see:

- [mdi-apps-framework : mdiXYPlot](https://github.com/MiDataInt/mdi-apps-framework/blob/main/shiny/shared/session/modules/widgets/plots/specific/mdiXYPlot)
