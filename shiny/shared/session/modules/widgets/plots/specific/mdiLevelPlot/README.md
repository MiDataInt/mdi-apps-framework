---
title: mdiLevelPlot
parent: Specific Plots
grand_parent: Display Widgets
has_children: false
nav_order: 30
---

## {{page.title}}

The **mdiLevelPlot** widget creates a level plot 
in a pre-existing plot frame
from an input `data.table` 
with an appropriate color legend. 

A level plot
is similar to a heat map in that it displays three-dimenional
data as a grid of rectangles, where a rectangle's color denotes its
Z-axis value. However, unlike heat maps, a level plot itself
does not perform any clustering of rows and columns. It 
plots data on two numeric, not categorical, axes. 

The plot and legend
are typically wrapped in a `staticPlotBox`, but `mdiLevelPlot` itself
does not provide the box, it provides functions that can
be used to fill the box. This organization can allow users to switch
between different three-dimensional data representations.

### mdiLevelPlot settings

To help users set level plot display attributes, you can access the
`mdiLevelPlotSettings` list to obtain a template for
the two mdiLevelPlot settings, `Level_Plot$Max_Z_Value` and
`Level_Plot$Level_Palette`, an RColorBrewer palette name. See the example below.

### Calling mdiLevelPlot

The `mdiLevelPlot` function takes the following arguments:

```r
# mdiLevelPlot.R
mdiLevelPlot <- function(
    dt, 
    xlim,  
    xinc,
    ylim,
    yinc,
    z.fn,
    z.column,
    settings,
    legendTitle,
    legendRoundDigits = 1,
    h = NULL,
    v = NULL,
    border = NA,
    ...
)
```

where:

- **dt** = a data.table or data.frame with at least columns x, y, and the column named by z.column
- **xlim** = the plot X-axis limits
- **xinc** = the regular increment of the X-axis grid
- **ylim** = the plot Y-axis limits 
- **yinc** = the regular increment of the Y-axis grid 
- **z.fn** = function applied to z.column, per grid spot, to generate the output color
- **z.column** = the column in dt passed to z.fn, per grid spot
- **settings** = a settings object from the enclosing staticPlotBox, or any list compatible with mdiLevelPlotSettings
- **legendTitle** = header for the color legend
- **h** = Y-axis values at which to place line rules
- **v** = X-axis values at which to place line rules
- **border** = passed to rect, suppresses grid borders by default
- **...** = additional arguments passed to rect()

Calling `mdiLevelPlot()` renders the plot by calling `rect()`, so your calling code must already
have initialized an empty plot frame, e.g., by calling `plot()` or `staticPlotBox$initializeFrame()`.

### mdiLevelPlot types: seq and div

`mdiLevelPlot` supports two kinds of Z-axis calculations, which
are determined by the value chosen for `Level_Plot$Level_Palette`. 
These two patterns are named according to the conventions of RColorBrewer, 
where palettes are of types `seq` or `div`.

In either case, data are equally divided into Z rank categories
from 0 to `abs(Level_Plot$Max_Z_Value)`.  The lowest values will
get the lightest colors according to the palette, highest values
get the darkest colors. Any grid position value beyond `Level_Plot$Max_Z_Value`
get the darkest color.

When a `seq` 'sequential' type palette is selected, e.g., the default Blues palette,
data are coerced to range from 0 to positive values. Any negative values in the data are made positive by `abs()`. Thus, colors report on the non-directional deviation from zero.

When a `div` 'divided' type palette is selected, 
data are assumed to range from negative to positive values. 
Thus, two or three different colors report on the symmetrical directional deviation from zero.

A legend is always added to the right plot margin to reveal the numerical meaning of colors.

### Supplying data to the widget

The data.table provided to `mdiLevelPlot` must have x, y, and z.column columns.
The widget will use `data.table` to aggregagate z.column by x and y. 
The color value applied to each pair of x and y coordinates is determined
by applying z.fn to z.column for that group, where z.fn should return
a numerical value.

Thus, `mdiLevelPlot` makes it easy to 
aggregate data on the fly to determine the output Z colors.

XY grid spots with no matching data in dt will have no rectangle 
plotted, and thus will be the same color as the plot background, typically white.

### Typical usage

```R
myPlot <- staticPlotBoxServer(
    "myPlot",
    margins = TRUE,
    settings = mdiLevelPlotSettings,
    create = function() {
        # reactive code to establish the plot data and limits
        myPlot$initializeFrame(...)
        mdiLevelPlot(...)
    }
)
```

### Additional references
 
For more detailed views of the module's code, see:

- [mdi-apps-framework : mdiLevelPlot](https://github.com/MiDataInt/mdi-apps-framework/blob/main/shiny/shared/session/modules/widgets/plots/specific/mdiLevelPlot)
