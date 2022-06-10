---
title: Sidebar Info
parent: User Feedback
has_children: false
nav_order: 30
---

## {{page.title}}

The main page sidebar isn't the best place for user feedback in
MDI apps since the space is needed 
for other framework items, but it is possible using the **sidebarInfoBox**
widget.

The following code block shows the basic call structure.

```r
sibebarInfoBoxUI( # in a module UI function
    id, 
    supertitle = "" # text shown above the value
) 
sibebarInfoBoxServer( # in a module server function
    id, 
    value, # a character value, or a function that returns one
    ... # additional arguments passed to a value function
)
```
