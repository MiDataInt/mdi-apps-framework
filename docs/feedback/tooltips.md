---
title: Tooltips
parent: User Feedback
has_children: false
nav_order: 20
---

## {{page.title}}

Sometimes you will wish to put context-dependent help
on specific UI items using the common **tooltip** mechanism.
The MDI apps framework provides a wrapper around the R
`tippy` package for server functions to use to add
tooltips to UI elements by id.

The following code block shows the basic call structures.

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
