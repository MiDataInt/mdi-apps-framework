---
title: Return Value
parent: Settings Panels
has_children: false
nav_order: 40
---

## {{page.title}}

Like many R Shiny modules, the `settings` widget returns
a list of reactives and methods to allow the caller 
to react to and manipulate settings.

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

where each option is accessed in the list as follows (notice
that the settings family, i.e., tab, is a reactive function!):

```r
# <module>_server.R
settings <- settingsServer(...)
value <- settings$<familyName>()$<settingName>$value # !! notice the () !!
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
