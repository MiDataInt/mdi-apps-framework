---
title: interactivePlots
parent: Plots
grand_parent: Display Widgets
has_children: false
nav_order: 20
---

{% include table-of-contents.md %}

<!-- ## {{page.title}} -->

**interactivePlots** is a family of widgets
that display interactive data plots generated with the 
[plotly](https://plotly.com/r/) javascript library.
"Interactive" means that user can select data points, adjust
axis limits, and more, in their browser.

Interactive plots are a feature of many well-designed
apps, but aren't always required or the best tool. 
If WYSIWYG ("what you see is what you get") images 
are more important, try the 
[staticPlotBox](/mdi-apps-framework/shiny/shared/session/modules/widgets/plots/staticPlotBox/README.html).

### How plotly works

When you fill a standard R Shiny `plotOutput`, 
the plot is generated on the server and 
sent as image data to the browser. Every time the user
wants to modify the image a request is sent to the server,
which must spend time generating the revised image before
sending it back. Shiny has basic image interaction
processes like click and sweep selection, but to act on 
them a request must again be sent to the server as the 
browser does not have the data, just an image.
Thus, it is a "server-side" approach.

The plotly library takes a "client-side" approach. The server
is responsible for assembling a data object with everything
needed to construct the plot that it sends to the browser.
The plotly javascript library, running in the web
browser, then renders to plot. Any interactions
with the plot, and any ensuing adjustments to the plot image,
are also handled in the browser, allowing for faster
updates, especially for complex plots.
The initial load of the plot can take a bit longer since
more data needs to be sent to the browser, but then
the experience is featured-rich and smooth.

### Ways to use plotly in MDI apps

You do not need to use the MDI framework's
widgets. Feel free to use the functions of the 
[R plotly package](https://cran.r-project.org/web/packages/plotly/index.html)
directly to make plots - the library is ready for use by all apps.

Here we describe wrappers around plotly
intended to make it a bit easier to use a complex library.

The main interactive plot widgets are:

- **interactiveBarplot** = vertical or horizontal bar plots, with optional stacking
- **interactiveScatterplot** = X-Y plots of one or more series of data points

### Basic plots - start simple and build 

Even though they make multi-part interactive plots easier to assemble,
the `interactivePlots` widgets are still fairly complex.
However, all options have defaults that allow you to generate initial
interactive plots by providing only the `plotData` argument.
We recommend starting from there and adding to that base to
build toward a richer data representation.

The following illustrates a very simple page
with one of each kind of interactive plot
(only widget-related code is shown):

```r
# <scriptName>_ui.R
interactiveBarplotUI(ns('bar'))
interactiveScatterplotUI(ns('scatter'))
```

```r
# <scriptName>_server.R
barData <- reactive({
    data.frame(
        value = 1:4,
        group = c("Group1", "Group1", "Group2", "Group2"),
        subgroup = c("Subgroup1", "Subgroup2", "Subgroup1", "Subgroup2"),
        stringsAsFactors = FALSE
    )
})
xyData <- reactive({
    data.frame(
        x = 1:4,
        y = 1:4
    )
})
interactiveBarplotServer('bar', barData)
interactiveScatterplotServer('scatter', xyData)
```

### Common interactivePlots UI options

The interactivePlots UI functions take the following arguments in addition to 'id':

```r
# interactiveXXXplotUI.R
interactiveBarplotUI <- function(id, height = '300px')
interactiveScatterplotUI <- function(id, height = '300px')
```

where all options are passed to `plotlyOutput()`:

- **height** = a valid height, typically in px, used to scale the size of the plot

### interactiveBarplotServer options

The `interactiveBarplotServer` function takes the following arguments in addition to 'id':

```r
# interactiveBarplot_server.R
interactiveBarplotServer <- function(
    id,
#------------------------------------
    plotData, 
    shareAxis = list(), 
    shareMargin = 0,
    orientation = 'vertical', 
#------------------------------------
    range = NULL,
    xtitle = NULL,
    ytitle = NULL,
#------------------------------------
    subgroupColors = NULL,
#------------------------------------
    lines = NULL,
    lineWidth = 2,
#------------------------------------
    clickable  = FALSE
)
```

where the following arguments define the plot data and structure:

- **plotData** = a reactive that returns a data.frame with columns 'value', 'group' and 'subgroup', a named vector of values, or a named list of such objects
- **shareAxis** = a list where x=TRUE or y=TRUE (not both) will cause >1 datasets to share that axis
- **shareMargin** = amount of space between stacked plots
- **orientation** = the orientation of the plotted bars, either horizontal or vertical

the following arguments define the axis properties:

- **range** = override the automatic range for the one numeric axis
- **xtitle** = x-axis label, character vector or a reactive
- **ytitle** = y-axis label, character vector or a reactive

the following arguments annotate data subgroups:

- **subgroupColors** = a color name or color palette used to color the subgroupings within each group

the following arguments create straight line overlays:

- **lines** = a function, reactive, or vector of axis values on the numeric axis where rules are drawn, or "mean" or "median"
- **lineWidth** = the width applied to `lines`

and the following arguments define the plot interactions sent back to the R server (see below):

- **clickable** = whether plot should react to bar clicks

For bar plots, if `plotData` is a named list, then a stack of plots is created.

### interactiveScatterplotServer options

The `interactiveScatterplotServer` function takes the following arguments in addition to 'id':

```r
# interactiveScatterplot_server.R
interactiveScatterplotServer <- function(
    id,
    plotData,
    accelerate = FALSE,
    shareAxis = list(),
    shareMargin = 0,
#------------------------------------
    mode = "markers",
    color = NA,
    symbol = NA,   
    pointSize = 3,
    lineWidth = 2,
#------------------------------------
    overplot = NULL,
    overplotMode = NULL,
    overplotColor = NA,
    overplotPointSize = 3, 
    overplotLineWidth = 2,
#------------------------------------
    xtitle = "x",
    xrange = NULL,
    xzeroline = TRUE,
    ytitle = "y",
    yrange = NULL,
    yzeroline = TRUE,
#------------------------------------
    ticks = list(x = NULL, y = NULL),
    grid = list(x = TRUE, y = TRUE), 
    selectable = FALSE,s
    clickable  = FALSE,
    keyColumn = NULL,
#------------------------------------
    hoverText = NULL, 
    labelCol = NULL, 
    labelDirs = list(x = 1, y = 1), 
#------------------------------------
    fitMethod = NULL, 
    fitColor = NA,
#------------------------------------
    unityLine = FALSE,
    hLines = NULL,
    vLines = NULL,
#------------------------------------
    distributions = NULL,
#------------------------------------
    cacheReactive = NULL
)
```

where the following arguments define the plot data and structure:

- **plotData** = data to plot, a reactive that returns a data.frame with $x and $y, or a named list of such data.frames
- **accelerate** = if TRUE, use scattergl/WebGL (instead of SVG) to plot large data series much more quickly (with limitations)
- **shareAxis** = list where x=TRUE or y=TRUE (not both) will cause >1 datasets to share that axis; incompatible with overplotting or fitting
- **shareMargin** = the space between stacked plots

the following arguments define the properties of the main data points or lines:

- **mode** = how to plot; 'markers', 'lines', etc.
- **color** = colors, usually left as NA, i.e., default colors
- **symbol** = a vector or named list of symbols, or a column name in plotData()
- **pointSize** = a vector or named list of point sizes, or a column name in plotData()
- **lineWidth** = the width of data lines

the following arguments define the properties of repeated/extra points plotted on top of the original points:

- **overplot** = repeated/extra points plotted on top of the original points; if character, column of that name becomes the trace number
- **overplotMode** = analogous to mode, for `overplot`; if plotData is a named list, overplot must be NULL or an equal length list with the same names; defaults to the same mode as the main plot
- **overplotColor** = analogous to color, for `overplot`
- **overplotPointSize** = analogous to pointSize, for `overplot`
- **overplotLineWidth** = analogous to lineWidth, for `overplot`

the following arguments define the axis properties:

- **xtitle** = x-axis label, character vector or a reactive
- **xrange** = override the automatic x-axis range
- **xzeroline** = whether or not to show a line at x = 0
- **ytitle** = y-axis label, character vector or a reactive
- **yrange** = override the automatic y-axis range
- **yzeroline** = whether or not to show a line at y = 0

the following arguments define the plot's ticks and grid:

- **ticks** = either NULL (default) or list(tick0=#, dtick=#) for the x (vertical) and y (horizontal) grids
- **grid** = either TRUE (default), FALSE (omitted), or a color value for the x (vertical) and y (horizontal) grids

the following arguments define the plot interactions sent back to the R server:

- **selectable** = whether point selection is enabled; either FALSE, TRUE (defaults to box select), 'select' (same as TRUE), 'lasso', 'h', or 'v'
- **clickable** = whether plot should react to point clicks
- **keyColumn** = the name of the column in plotData to add as a click/select key; ends up in 'customdata' field of event_data

the following arguments define the labeling of data points:

- **hoverText** = character vector with hover text, or a function or reactive that returns one; if a 1-length character vector, hoverText is taken from that column of plotData()
- **labelCol** =  the name of a column from which to read the text labels applied to a subset of points (use NA for unlabeled points)
- **labelDirs** = direction to draw the label arrow relative to x,y; 0=no offset, 1=farther along the axis, -1=opposite of 1 (i.e, to the inside) 

the following arguments support curve fitting to the data points:

- **fitMethod** = a reactive that supplies a fit, a function(d) that returns a fit, or a method compatible with `fitTrendline`
- **fitColor** = color of the curve fit

the following arguments add reference lines:

- **unityLine** = add a unity line after plotting the points
- **hLines** = a function, reactive, or vector of y-axis values for horizontal rules
- **vLines** = a function, reactive, or vector of x-axis values for vertical rules

the following arguments add data distributions:

- **distributions** = a function that returns a list of data.frames with $x and $y to plot as individual grey, dashed line distribution traces 

and the following arguments create a key that is used to cache plots for faster server-side updates:

- **cacheReactive** = optional reactive with (hopefully simple to parse) values on which the plot depends; passed to `bindCache` as cache keys

### interactiveBarplotServer return values

The `interactiveBarplotServer` module returns a list as follows:

```r
# interactiveBarplot_server.R
list(
    clicked = clicked
)
```

where **clicked** is a `reactiveVal` that returns `plotly::event_data("plotly_click")`
so that your module can react when a user clicks a data bar.

### interactiveScatterplotServer return values

The `interactiveScatterplotServer` module returns a list as follows:

```r
# interactiveScatterplot_server.R
list(
    selected = selected,
    clicked = clicked,
    fit = fit
)
```

where:
- **selected** = a `reactiveVal` that returns `plotly::event_data("plotly_selected")` so that your module can react when a user selects one or more data points
- **clicked** = a `reactiveVal` that returns `plotly::event_data("plotly_click")`
so that your module can react when a user clicks a data point
- **fit** = a `reactiveVal` that returns the results of `fitMethod` applied to `plotData`

### Plot stacking of multiple data sets

It is easier to read plots with multiple data sets, and takes
less space, when they are shown in a stacked fashion with a shared
x (or y) axis. This is accomplished by setting `plotData` to be 
a named list of data.frames (rather than a single data frame)
and using the `shareAxis` and `shareMargin` arguments to communicate
whether to share an axis and how much space to put between shared-axis plots.

### Overplot method for highlighting data points

You can choose to highlight data points in scatterplots by adjusting
the point properties in `plotData`, `pointSize`, etc. 
However, this can be annoyingly difficult to get right, and 
unsatisfying in high-density plot where points get lost in the background.

`interactiveScatterplot` offers an alternative approach in which
you select a subset of data points in `overplot` to plot a second time, 
on top of the primary plotting of `plotData`. The points are thus
configured entirely on their own and guaranteed to be visible.

### Using plot interactions server-side

Many outcomes of a user's interactions
with your plot are handled client-side by plotly. However,
many times you will also want to access them server-side, in your module.

The `selected` and `clicked` return values provide reactiveVals that 
your code can use to take appropriate actions. The contents of those reactiveVals
are described in the plotly documentation, or simply use `str(myPlot$selected())`
when developing code to see what's inside.

Common examples of things done in response to a plot interaction would
be to update the plot itself or another plot or table that depends on user selections.

### Additional references

For more detailed views of the modules' code, see:

- [mdi-apps-framework : interactivePlots](https://github.com/MiDataInt/mdi-apps-framework/blob/main/shiny/shared/session/modules/widgets/plots/interactivePlots)

The following appStep server has several complete working examples:

- [svx-mdi-tools : normalizeGC_server](https://github.com/wilsontelab/svx-mdi-tools/blob/main/shiny/apps/wgaSeq/modules/appSteps/normalizeGC/normalizeGC_server.R)
