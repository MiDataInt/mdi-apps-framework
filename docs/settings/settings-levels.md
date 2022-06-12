---
title: Settings Levels
parent: Settings Panels
has_children: false
nav_order: 10
---

## {{page.title}}

A settings widget can be attached to 
an appStep page or to any box or widget in your UI. 

### Step-level settings

Many appStep modules define settings that
are relevant to most or all of the output elements on its page.
This is achieved by first defining the required settings in _module.yml_:

```yml
# <appStep>/module.yml
settings:
    <settingsTabName>:
        <settingName>: # see syntax descriptions below
        # etc.
```

then activating the setting icon link in _ui.R_:

```r
# <appStep>/<appStep>_ui.R
<appStep>UI <- function(id, options) {
    ns <- NS(id)    
    standardSequentialTabItem(
        HTML(paste( options$longLabel, settingsUI(ns('settings')) ))
        # etc.
    )
}
```

and finally, activating the settings server in _server.R_ and returning
its value to be stored in bookmarks and potentially used by other steps:

```r
# <appStep>/<appStep>_server.R
<appStep>Server <- function(id, options, bookmark, locks) {
    moduleServer(id, function(input, output, session) {
    settings <- settingsServer( 
        id = 'settings',
        parentId = id
         # see additional options below
    )
    # ...
    list(
        settings = settings$all_
    )

})}
```

### Widget-level settings

A similar expandable settings icon can be added to any widget
by making edits to its module files similar
to the special case of appStep modules shown above. Just
decide what options you need, where you want to place the 
`settingsUI()` icon in the widget's UI, activate the `settingsServer()`,
and the rest is handled by the framework. Remember to add
the settings object to the widget's return values if a parent module 
using your widget needs to access them.

```r
# <widget>/<widget>_server.R
<widget>Server <- function(id) {
    moduleServer(id, function(input, output, session) {
    settings <- settingsServer( 
        id = 'settings',
        parentId = id
         # see additional options below
    )
    # ...
    list(
        settings = settings
    )
})}
```

### Naming a settings widget

We recommend sticking with the simple, descriptive
id of 'settings' for a single instance of the `settings` widget added to your
module. In the unusual circumstance
when you have two settings icons in the same module, i.e., in the
same namespace, they must have different names, like 
'plotSettings' and 'tableSettings'.
