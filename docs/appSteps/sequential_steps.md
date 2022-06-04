---
title: Sequential Steps
parent: App Steps
has_children: false
nav_order: 30
---

## {{page.title}}

In many cases it won't make sense for
an app step to be active until a previous step has been completed, e.g.,
if it depends on user inputs from the previous step.
A specific portion of an appStep's configuration and module return
values enforce this sequential dependency. 

### App step dependency chains

MDI apps use appStep type declarations to create a dependency 
of one or more modules on one or more previous modules, as follows:

```yml
# <appStep>/module.yml
types: # the type(s) assigned to this appStep
    - myType
sourceTypes: # the module type(s) on which it depends (is a child of)
    - parentType
```

Note that these are arrays of types and can be multiple,
but most apps use just one type for an appStep and its parent 
to create a linear step dependency chain.

### Step readiness states in appStep module return values

A child step knows it can be displayed because its parent
declares an **isReady** reactive, or another function with no arguments,
in its return value, as follows:

```r
# <appStep>/<appStep>_server.R
appStepServer <- function(id, options, bookmark, locks) {
    moduleServer(id, function(input, output, session) {
    list( # the module's return value
        isReady = reactive({ getStepReadiness(source = options$source, ...) })
    )
})}
```

Arguments for the **getStepReadiness** function are:

- **source** = the parent appStep, communicated as `options$source`
- **list** = a list that must have length > 0 to be considered ready
- **fn** = a function that returns a logical ready state
- **...** = additional arguments passed to fn

See also:

- [sequentialMenu.R getStepReadiness()](https://github.com/MiDataInt/mdi-apps-framework/blob/main/shiny/shared/session/ui/)