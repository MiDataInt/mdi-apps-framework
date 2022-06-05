---
title: Settings Panels
has_children: false
nav_order: 40
---

## {{page.title}}

The MDI apps framework encourages (but never demands) an uncluttered
UI for your app. In part, this means not placing dozens 
of option inputs onto the main UI page. We encourage you to expose
inputs to users as they need them in an organized fashion.

We help achieve this through a YAML-based
"settings" module, whose resulting gear/cog icon can be attached to 
an appStep page or to any box or widget in your UI. 
When clicked, the icon opens a modal popup revealing structured option inputs.
Users quickly become accustomed to looking under that icon for context-relevant options.

Disadvantages are a small delay to being
able to edit options and that the values in force are not always
evident on screen. The few inputs where these concerns become large,
e.g., frequently modified, critically important user choices, should 
be extracted from settings and shown as one of the few on-page inputs.

### Step-level settings

Many appStep modules define settings that
are relevant to most or all of the output elements on its page.
This is achieved by first defining the required settings in module.yml:

```yml
# <appStep>/module.yml
settings:
    <settingsTabName>:
        <settingName>: # see syntax descriptions below
        # etc.
```

then activating the setting icon link in ui.R:

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

and finally, activating the settings server in server.R and returning
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

### Creating settings definitions in module.yml

Settings popups are organized into input families, where
each named family is shown on its own tab in the modal panel.
Each input has its own unique ID.

The syntax for defining inputs will be familiar to anyone
creating R Shiny pages, as it captures the names
and most important options of Shiny inputs into YAML format:

```yml
settings:
    <settingsTabName>:
        <settingName>:
            type:   textInput
            value:  abc  
        <settingName>:
            type:   numericInput
            value:  1
            min:    1
            max:    100
            step:   1 
        <settingName>:
            type: selectInput
            choices:
                - abc
                - xyz
            value: abc    
        <settingName>:
            type: radioButtons
            choices:
                - abc
                - xyz
            value: abc    
        <settingName>:
            type: checkboxGroupInput
            choices:
                - abc
                - xyz
            value: abc 
        <settingName>:
            type: checkboxInput
            value: true 
        <settingName>:
            type:   fileInput
            accept: 
                - ".csv"
                - ".txt"
```

### Naming settings inputs and input families

The `settings` widget uses a convenience shortcut to prevent
you from having to enter both names and ids for settings.
The family/tab and input keys in YAML 
are used directly as the on-screen labels in the settings popup,
after replacing all underscores with spaces. 
This typically means you should use
title case for family and settings keys, e.g., "Plot_Parameters"
and "X_Axis".

### settingsUI options

The `settingsUI` function takes a single argument in addition to 'id':

```r
# settings_ui.R
settingsUI <- function(id, isHeader = TRUE)
```

where setting **isHeader** to TRUE reduces the icon size in a manner
appropriate for inclusion in a step header or box title,
which is the most appropriate placement in many widgets.

### settingsServer options

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

### settingsServer return value list

The following values are returned by all instances of the `settings`
widget to allow you to read and modify settings.

```r
# settings_server.R
retval <- reactiveValuesToListOfReactives(settings) # the categorized settings reactives
retval$all_ <- reactive({ allSettings() })
retval$replace <- initializeSettings
retval$cache <- reactive({ cache })
retval$get <- function(tab, id){
    x <- settings[[tab]]
    if(is.null(x)) return(NULL)
    x <- x[[id]]
    if(is.null(x)) return(NULL)
    x$value
}
retval
```

where each option is accessed in the list as follows (note carefully
that the settings family, i.e., tab, is a reactive function!):

```r
# <module>_server.R
settings <- settingsServer(...)
value <- settings$<familyName>()$<settingName>$value # notice the () !
value <- settings$Plot_Parameters()$X_Axis$value
```

and additional helper elements are:

- **all_** = a reactive to all settings, mainly used for building bookmarks
- **replace** = a function to replace all settings en bloc in code, mainly used to load bookmarks
- **cache** = not described here, for advanced use
- **get** = a convenient, ofter clearer, alternative for retrieving values

thus supporting calls like:

```r
# <module>_server.R
settings <- settingsServer(...)
value <- settings$get("Plot_Parameters", "X_Axis")
```
