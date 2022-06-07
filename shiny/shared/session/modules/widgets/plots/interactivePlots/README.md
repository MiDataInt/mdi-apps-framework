---
title: interactivePlots
parent: Plots
grand_parent: Display Widgets
has_children: false
nav_order: 20
---

## {{page.title}}

**interactivePlots** is a family of widgets
that display interactive data plots generated with the 
[plotly](https://plotly.com/r/) javascript library.
"Interactive" means that user can select data points, adjust
axis limits, and similar activities in their browser.

Interactive plots are a key feature of many well-designed
apps that make data "come alive", but they aren't always
required or the best tool. If WYSIWYG ("what you
see is what you get") images are more important, try
the staticPlotBox widget.

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
The plotly javascript library, running entirely in the web
browser, then renders to plot for the user. Any interactions
with the plot, and any ensuing adjustments to the plot image,
are also handled in the browser, allowing for faster
updates in an interactive session, especially for complex plots.
The initial load of the plot can take a bit longer since
more data needs to be sent to the browser, but then
the experience is featured-laden and smooth.

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

### Start simple and build 

Even though they make multi-part interactive plots easier to assemble,
the interactivePlots widgets are still fairly involved with many arguments.
However, nearly all options have defaults that allow you to generate simple
interactive plots by providing nothing more than the `plotData` argument.
We recommend starting from there and adding to that base as
you build toward a richer data representation.

### Common interactivePlots UI options

The interactivePlots UI functions take the following arguments in addition to 'id':

```r
# interactiveXXXplotUI.R
interactiveScatterplotUI <- function(id, height = '300px')
interactiveBarplotUI <- function(id, height = '300px')
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

and the following arguments define the plot interactions sent back to the R server:

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

- **plotData** = data to plot, a reactive that returns data.frame with $x and $y, or a named list of such data.frames
- **accelerate** = if TRUE, use scattergl/WebGL (instead of SVG) to plot large data series much faster (with limitations)
- **shareAxis** = list where x=TRUE or y=TRUE (not both) will cause >1 datasets to share that axis; incompatible with overplotting or fitting
- **shareMargin** = the space between stacked plots

the following arguments define the properties of the main data elements:

- **mode** = how to plot; markers, lines, etc.
- **color** = colors, usually left as NA, i.e., default colors
- **symbol** = a vector or named list of symbols, or a column name in plotData()
- **pointSize** = a vector or names list of point sizes, or a column name in plotData()
- **lineWidth** = the width of lines

the following arguments define the properties of repeated or extra points plotted on top of the original points:

- **overplot** = repeated or extra points plotted on top of the original points; if character, column of that name becomes the trace number
- **overplotMode** = analogous to mode, for overplot; if plotData is a named list, overplot must be NULL or an equal length list with the same names; defaults to the same mode as the main plot
- **overplotColor** = analogous to color, for overplot
- **overplotPointSize** = analogous to pointSize, for overplot
- **overplotLineWidth** = analogous to lineWidth, for overplot

the following arguments define the axis properties:

- **xtitle** = x-axis label, character vector or a reactive
- **xrange** = override the automatic x-axis range
- **xzeroline** = whether or not to show put a line at x = 0
- **ytitle** = y-axis label, character vector or a reactive
- **yrange** = override the automatic y-axis range
- **yzeroline** = whether or not to show put a line at y = 0

the following arguments define grid and ticks:

- **ticks** = either NULL (default) or list(tick0=#, dtick=#) for the x (vertical) and y (horizontal) grids
- **grid** = either TRUE (default), FALSE (omitted), or a color value for the x (vertical) and y (horizontal) grids

the following arguments define the plot interactions sent back to the R server:

- **selectable** = whether point selection is enabled; either FALSE, TRUE (defaults to box select), 'select' (same as TRUE), 'lasso', 'h', or 'v'
- **clickable** = whether plot should react to point clicks
- **keyColumn** = the name of the column in plotData to add as a click/select key; ends up in 'customdata' field of event_data

the following arguments define the labeling of data points:

- **hoverText** = character vector with hover text, or a function or reactive that returns one; if a 1-length character vector, hoverText taken from that column
- **labelCol** =  the name of a column from which to read the text labels applied to a subset of points (use NA for unlabeled points)
- **labelDirs** = direction to draw the label arrow relative to x,y; 0=no offset, 1=farther along the axis, -1=opposite of 1 (i.e, to the inside) 

the following arguments supporting curve fitting to the data points:

- **fitMethod** = a reactive that supplies a fit, a function(d) that returns a fit, or a method compatible with fitTrendline
- **fitColor** = color of the curve fit

the following arguments add reference lines:

- **unityLine** = add a unity line after plotting the points
- **hLines** = a function, reactive or vector of y-axis values
- **vLines** = a function, reactive or vector of x-axis values

the following arguments add data distributions:

- **distributions** = a function that returns a list of data.frames with $x and $y to plot as individual grey, dashed line distribution traces 

and the following arguments create a key that is used to cache plot for faster server-side updates:

- **cacheReactive** = optional reactive with (hopefully simple to parse) values on which the plot depends; passed to bindCache as cache keys






For more detailed information, see:

[mdi-apps-framework : interactivePlots](https://github.com/MiDataInt/mdi-apps-framework/blob/main/shiny/shared/session/modules/widgets/plots/interactivePlots)
