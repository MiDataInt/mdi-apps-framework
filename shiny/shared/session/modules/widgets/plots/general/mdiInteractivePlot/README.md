---
title: mdiInteractivePlot
parent: General Plots
grand_parent: Display Widgets
has_children: false
nav_order: 30
---

## {{page.title}}

The **mdiInteractivePlot** plot widget displays a plot
that is drawn as a static plot or image, but offers a
set of interactive features implemented using a combination
of Shiny and javascript code. These features include hover, click,
and drag actions by the user, and a convenient crosshairs over all plots.

Unlike 
[interactivePlots](/mdi-apps-framework/shiny/shared/session/modules/widgets/plots/interactivePlots/README.html)
widgets, use interactions that result in changes to the plot will completely re-render 
the plot server-side, i.e., mdiInteractivePlot is a server, not a client-side widget.

### mdiInteractivePlotUI options

The `mdiInteractivePlotUI` function takes no arguments other than 'id':

```r
# mdiInteractivePlot_ui.R
mdiInteractivePlotUI <- function(id)
```

### mdiInteractivePlotServer options

The `mdiInteractivePlotServer` function takes the following arguments in addition to 'id':

```r
# mdiInteractivePlot_server.R
mdiInteractivePlotServer <- function(
    id,   
    hover = TRUE,    
    click = TRUE,
    brush = TRUE,
    delay = 500,
    contents = NULL # a reactive that returns:
    # contents = reactive({ list(
    #     pngFile = path, # OR plotArgs
    #     plotArgs = list(
    #          ... # first element must have a "plot.class" method and be named "x" or unnamed
    #     ),
    #     layout = list(
    #         width = pixels,
    #         height = pixels,
    #         pointsize = integer, # defaults to 8
    #         dpi = integer, # defaults to 96
    #         mai = integer vector,
    #         xlim = range, # OR can be read from plotArgs
    #         ylim = range
    #     ),
    #     abline = list(), # optional named arguments passed to abline() after calling plot(plotArgs)
    #     parseLayout = function(x, y) list(x, y, layout) # to convert to plot space in a multi-plot layout
    # ) })
)
```

where:

- **hover** = a logical whether the plot will have hover actions, i.e., when the user holds the mouse over something
- **click** = a logical whether the plot will have click actions, i.e., when the user clicks a point
- **brush** = a logical whether the plot will have brush actions, i.e., when the user sweeps an area
- **delay** = the number of milleseconds to wait before activating brush actions
- **contents** = a reactive that provides information on how to render the plot; see commented code above


### mdiInteractivePlotServer return values

The module returns a list as follows:

```r
# mdiInteractivePlot_server.R
list(
    hover = hover,    
    click = click,
    brush = brush,
    pixelToAxes = pixelToAxes,
    pixelToAxis = pixelToAxis
)
```

where:

- **hover** = a reactive that returns the details of a user hover event
- **click** = a reactive that returns the details of a user click event
- **brush** = a reactive that returns the details of a user brush event
- **pixelToAxes** = fuction of signature `function(x, y)` that changes XY pixels coordinates to plot coordinates
- **pixelToAxis** = fuction of signature `function(x, layout, leftI, rightI, dim, lim, invert)`

Please use `str(click())` and similar while developing code to see the format and metadata of each event.

### Using the widget

First, place an instance of the mdiInteractivePlot widget in your UI 
(only widget-related code is shown):

```r
# <scriptName>_ui.R
mdiInteractivePlotUI(ns('id'))
```

Then activate the plot in the matching server and define
the function that will fill (i.e., create) the plot:

```r
# <scriptName>_server.R
myPlot <- mdiInteractivePlotServer(
    'id', 
    hover = FALSE,
    click = TRUE,
    brush = FALSE,
    contents = reactive({ list(
        pngFile = "/path/to/my/png.png", 
        layout = list(
            width = 600,
            height = 600,
            mai = c(0.5, 0.5, 0.1, 0.1),
            xlim = c(0, 1), 
            ylim = c(0, 10)
        )
    ) })
)
```

Importantly, there are two ways that your desired plot can be communicated to the widget.
First, as illustrated above, your code can completely handle plot rendering to create
the png image file that is passed to the browser. 

Alternatively, you can provide a list of plotting arguments, as would normally be passed
to the R `plot()` command. The widget will make the call to `plot()` and
create the png file for you.

In either case, it is important to accurately communicate the layout of the plot to the widget,
as these parameters allow accurate conversion between plot pixels and axis values. If you get your layout wrong,
your click and other actions will not return correct values.

### Additional references
 
For more detailed views of the module's code, see:

- [mdi-apps-framework : mdiInteractivePlot](https://github.com/MiDataInt/mdi-apps-framework/blob/main/shiny/shared/session/modules/widgets/plots/general/mdiInteractivePlot)

For a complete working example, see:

- [genomex-mdi-tools : trackBrowser](https://github.com/wilsontelab/genomex-mdi-tools)
