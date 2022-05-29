---
published: false
---

## Shiny Modules

**modules** are R Shiny modules that define reusable UI components
and associated server logic for app steps, widgets, etc.

A module defines one discrete, reusable part of the UI that has both 
UI elements and code logic. Please read about Shiny modules online 
to understand how they work.

By convention, MDI module scripts follow the naming convention:

- **module_ui.R** - carries the function that returns the module's UI elements
- **module_server.R** - carries the function that defines the module's actions
- **module_utilities.R** - additional support functions for the module

Like any R functions, module server functions return a single
object that can be used by the calling code. Typical return values
are a list of some combination of reactive objects and method functions 
to be applied to the component or its data.

### Module types

Modules have different roles in MDI apps:

- **appControl** - provide common actions of the framework,
such as bookmarking and history caching.

- **appSteps** - define the sequential actions displayed
on the leftmost dashboard tabs of the MDI web page. See the appSteps 
README.md for details about their specific return value structure.

- **widgets** - define UI elements, panels, plots, etc. that
might be placed once or multiple times across any app.

### Module script templates

The following code blocks show the structure of a module's UI and server scripts. 
Replace "myModule" with the name of your module in the script
and function names. See existing framework modules for extended examples.

Module UI functions must use the Shiny <code>ns()</code> function to wrap
UI element names, which causes them to have properly nested and addressable
names.

```
# module ui function, in myModule_ui.R

myModuleUI <- function(id, ...) {

    # initialize namespace
    ns <- NS(id) 

    # add Shiny UI elements here
    textInput(ns('inputName'))
    textOutput(ns('outputName'))
}
```

UI elements are then addressable in the module's 
<code>input</code> object using simply the name provided in the UI function.

```
# module server function, in myModule_server.R

myModuleServer <- function(id, ...) {
    moduleServer(id, function(input, output, session) {
#----------------------------------------------------------------------

# add Shiny server reactive actions here: observe, render, etc.
output$outputName <- renderText({
    input$inputName
})

# module return values go here (if none, use NULL instead of a list)
list(
    myValue = reactiveVal(input$inputName),
    myMethod = function(...) {}
)

#----------------------------------------------------------------------
})}
```

### Using a module

The following code blocks show how to use your module in calling scripts. Very often you will be nesting modules within modules - the examples show how to ensure that names are assembled and accessed correctly.

```
# within any UI script, e.g., parentModule_ui.R

myModuleUI(ns('moduleInstanceId'), ...)
```

Be sure that the name/id of the module matches between the UI and server calls.

```
# within any server script, e.g., parentModule_server.R

# use id, NOT ns(id), within the server function!

myObject <- myModuleServer('moduleInstanceId', ...)

output$xyz <- renderText({
    myObject$myValue() # <<< accesses the modules reactive return value
})
```
