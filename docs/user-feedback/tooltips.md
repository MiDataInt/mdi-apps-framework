---
title: Tooltips
parent: User Feedback
has_children: false
nav_order: 20
---

## {{page.title}}

Sometimes it is beneficial to put context-dependent help
on specific UI items using the common **tooltip** mechanism.
The MDI apps framework provides wrappers around the 
[shinyBS package](https://cran.r-project.org/web/packages/shinyBS/index.html) 
for adding standardized tooltips to UI elements by id.

{% include figure.html file="user-feedback/tooltip.png" border=true %}

### Adding tooltips from server functions

The following code block shows the call structures that can be used
anywhere within a module server script:

```r
# <moduleName>_server.R
mdiTooltip( # a single tooltip
    session, 
    id, 
    title, 
    placement = "top", 
    ui = FALSE
)
mdiTooltips( # multiple tooltips added at once
    session, 
    tooltips, 
    ui = FALSE
)
```

where: 
- **session** = the session object of the calling module
- **id** = the id of the UI element that the tooltip is attached to
- **title** = the tooltip text
- **placement** = where to put the text relative to the element
- **ui** = caller is adding a tooltip from within a renderUI expression
- **tooltips** = a list of tooltips, each as character(id, title, [placement])

for example:

```r
# <moduleName>_server.R
mdiTooltip( # a single tooltip
    session, 
    id = 'elementId', 
    title = 'Important information about the element', 
    placement = "bottom"
)
```

### Adding tooltips from UI functions

The following code block shows the call structure that can be used
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
ns <- NS(id)
mdiTooltipUI(
    ns('elementId'), 
    'Important information about the element'
)
```

### Adding help icon (?) tooltips to Shiny inputs

A known limitation is that `mdiTooltip()` does not
work to place tooltips on Shiny inputs, e.g., `selectInput()`. Thankfully, 
that is rarely a good idea as you do not want popups to appear
every time a user interacts with an input, only when they want help.

Instead, the apps framework provides the `addInputHelp()` function
to add typical **?** icons to an input's label whose sole purpose is to provide 
context-dependent help.

{% include figure.html file="user-feedback/addInputHelp.png" border=true %}

The call structure is:

```r
# <moduleName>_server.R
addInputHelp(
    session, 
    id, 
    title
)
```

where: 
- **session** = the session object of the calling module
- **id** = the id of the Shiny input that the tooltip is attached to
- **title** = the tooltip text

for example:

```r
# <moduleName>_server.R
addInputHelp(
    session, 
    id = 'selectInputId', 
    title = 'Information about the data to be selected'
)
```
