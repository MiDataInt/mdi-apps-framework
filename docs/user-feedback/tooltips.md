---
title: Tooltips
parent: User Feedback
has_children: false
nav_order: 20
---

## {{page.title}}

Sometimes you will wish to put context-dependent help
on specific UI items using the common **tooltip** mechanism.
The MDI apps framework provides a wrapper around the 
[shinyBS package](https://cran.r-project.org/web/packages/shinyBS/index.html) 
for apps to use to add standardized tooltips to UI elements by id.

{% include figure.html file="user-feedback/tooltip.png" border=true %}

TODO: aspects of tooltip utilization seem erratic and may have changed in
recent versions of shinyBS; tooltip.R may require attention. 

### Adding tooltips from server functions

The following code block shows the basic call structures that can be used
anywhere within a module server script:

```r
# <moduleName>_server.R
mdiTooltip( # a single tooltip
    ns, 
    id, 
    title, 
    placement = "top", 
    ui = FALSE
)
mdiTooltips( # multiple tooltips added at once
    ns, 
    tooltips, 
    ui = FALSE
)
```

where: 
- **ns** = the namespace function of the calling environment
- **id** = the id of the UI element that the tooltip is attached to
- **title** = the tooltip text
- **placement** = where to put the text relative to the element
- **ui** = caller is adding a tooltip as part of a renderUI expression
- **tooltips** = a list of tooltips, each as character(id, title, [placement])

for example:

```r
# <moduleName>_server.R
mdiTooltip( # a single tooltip
    ns, 
    id = 'mySelectInput', 
    title = 'information about the data to be selected', 
    placement = "bottom"
)
```

### Adding tooltips from UI functions

The following code block shows the basic call structure that can be used
anywhere within a module UI script:

```r
# <moduleName>_ui.R
mdiTooltipUI(
    ns(id), 
    title, 
    placement = "top"
)
```

for example:

```r
# <moduleName>_ui.R
mdiTooltipUI(
    ns('mySelectInput'), 
    'information about the data to be selected'
)
