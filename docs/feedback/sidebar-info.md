---
title: Sidebar Info
parent: User Feedback
has_children: false
nav_order: 30
---

## {{page.title}}

In general the main page sidebar isn't the best place for
your app to give on-screen feedback since the space is needed 
for other framework items, but it is possible using the **sidebarInfoBox**
widget.

The following code block shows the basic call structure.

```r
sibebarInfoBoxUI(
    id, 
    supertitle = "" # text shown above the value
) 
sibebarInfoBoxServer(
    id, 
    value, # a character value, or a function that returns one
    ... # additional arguments passed to a value function
)
```
