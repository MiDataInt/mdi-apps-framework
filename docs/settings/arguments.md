---
title: Module Arguments
parent: Settings Panels
has_children: false
nav_order: 30
---

## settingsUI arguments

The `settingsUI` function takes a single argument in addition to 'id':

```r
# settings_ui.R
settingsUI <- function(id, isHeader = TRUE)
```

where setting `isHeader` to TRUE reduces the icon size in a manner
appropriate for inclusion in a step header or box title,
the most appropriate placement in many widgets.

## settingsServer arguments

The `settingsServer` function takes various arguments in addition to 'id':

```r
# settings_server.R
settingsServer <- function(
    id, 
    parentId, 
    templates = list(parentId),
    size = NULL,
    cacheKey = NULL,
    fade = FALSE,
    title = "Set Parameters",
    immediate = FALSE,
    resettable = TRUE 
)
```

where:

- **id** = the id of the settings widget
- **parentId** = the id of the module loading the widget
- **templates** = a list of one or more of: parentId, a path to a settings.yml file, or a similarly formatted list object
- **size** = force a size for the modal popup as s/m/l (otherwise, auto-set based on contents)
- **cacheKey** = a reactive/reactiveVal that returns an id for the current settings state
- **fade** = whether to show an entry animation delay on the modal popup
- **title** = a title for the modal popup
- **immediate** = if TRUE, setting changes are transmitted in real time (otherwise when the modal is closed)
- **resettable** = if TRUE, a 'Reset All Settings' link will be provided to restore to defaults

The inputs that will populate the settings modal popup are determined
from the list of `templates`, allowing you to merge settings enumerated
from potentially many sources. The default is to look in the module.yml
file of the parent module calling the settings widget.

The default behavior shows the popup without delay and transmits the new settings
values only when the user closes the modal, behaviors that can be changed as indicated.

>Be very careful setting `immediate = TRUE` if the actions triggered by 
settings changes are slow.
