---
published: false
---

## Shiny Modules

**appSteps** modules share the properties of all MDI/Shiny modules
but have a specific purpose of defining one sequential app
step listed on the dashboard sidebar.

In addition to the descriptions below, please see the standard
appStep modules for working examples.

## appStep UI function 

### Inputs

In addition to the standard Shiny 'id' argument, appStep UI functions must
take object 'options' as a second input argument:

```
# appStep module ui function, in myAppStep_ui.R

myAppStepUI <- function(id, options)
```

The options object is a nested R list with the module's configuration options,
as defined by the module's **module.yml** and the calling app's **config.yml** files.

### Element definition

appStep UI functions must create a single UI element as follows:

```
# appStep module ui function, in myAppStep_ui.R

standardSequentialTabItem(
    title,
    leaderText,
    ... # the UI element for the appStep tabbed page
)  
```

## appStep server function

### Inputs

In addition to the standard Shiny 'id' argument, appStep server functions must
take the following additional input arguments:

```
# appStep module server function, in myAppStep_server.R

myAppStepServer <- function(id, options, bookmark, locks)
```

where:

- **options** - the same configuration object as passed to the UI function
- **bookmark** - provides access to the server bookmark reactive
- **locks** - provides access to the server locks reactive

### Return values

appStep server functions must return a specifically structured list
as a return object to allow proper visibility control for sequential
actions and to ensure that proper values are stored in bookmarks.

```
# appStep module server function, in myAppStep_server.R

# return value
list(
    outcomes = list(   
        myOutcome = reactive()
    ),
    isReady = reactive({ getStepReadiness(...) }),
    ...
)
```

where:

- **outcomes** - a list of reactive objects for use by calling scripts that 
will additionally be stored in bookmarks
- **isReady** - a reactive or a function that returns a logical to indicate whether the appStep has been completed; later steps will be masked until isReady() returns TRUE

The list may contain any other objects or methods as needed, but they
will not be included in bookmarks unless they are a subset of the 'outcomes' key
and are reactive.
