---
title: App Step Configuration
parent: App Steps
has_children: false
nav_order: 10
---

## {{page.title}}

A first thing to do when developing your app is to 
plan and declare its appStep modules.

### App step declaration in \<app\>/config.yml

The steps for your app are listed in its _config.yml_ file,
following the instructions found in the template file and 
this example:

```yml
# <app>/config.yml
appSteps:
    <stepName>: # your name for the appStep
        module: <moduleName>     # expect moduleNameUI and moduleNameServer
        shortLabel: ...          # shown on dashboard menu
        shortDescription: ...    # shown on the Overview page
        options:
            longLabel: ...       # title for the appStep tabbed page
            alwaysVisible: false # override step dependency chains
            # module-specific options go here
    <stepName>:  # etc.
```

The _template app's _config.yml_ file has declarations for 
an initial app step sequence based on standardized modules
that is common to many apps, with support for
additional source file upload, sample assignments into groups, and
asynchronous data analysis.

### Step module default configuration in \<appStep\>/module.yml

An appStep module can also have standard declarations in its _module.yml_ file.
These act as defaults that can be overridden by an app, in addition
to other declarations discussed below. 

```yml
# <appStep>/module.yml
shortLabel: ...
shortDescription: ...
longLabel: ...
```

Due to declarations in _module.yml_, many times the appStep declarations
in _config.yml_ can be quite simple:

```yml
# <app>/config.yml
appSteps:
    stepName_1: 
        module: moduleName_1    
```

### Other declarations in \<appStep\>/module.yml

The other appStep-specific declarations in _module.yml_ are
**types** and **sourceTypes**, which establish step dependency chains
as discussed in detail later.

Other declarations permitted in an appStep _module.yml_ file are the same
as other component configuration files, including **packages**
and **settings**, discussed in detail later. Step-level settings
should be activated in the UI and server functions below to be shown
at the top on an appStep page.

### appStep-specific structure of a module UI function

The following template shows how to declare an appStep UI function 
by:
- extending the Shiny module function declaration with additional 
arguments
- wrapping the appStep UI contents with the required `standardSequentialTabItem()` function,
which constructs the appStep page structure

```r
# <appStep>/<appStep>_ui.R
appStepUI <- function(id, options) {
    ns <- NS(id) # the module's namespace
    standardSequentialTabItem(
        title,       # title for the appStep page
        leaderText,  # regular text below the title, above the UI elements
        xxInput(ns('id'))  # UI elements for the appStep's tabbed page
        # etc.
    )
}
```

where the required arguments in addition to the standard module instance id are:

- **options** = the assembled step options as defined in module.yml and config.yml

A common value for `title` exploits a combination of `longLabel`
and `settingsUI` to expose step-level settings with a gear icon:

```r
# <appStep>/<appStep>_ui.R
standardSequentialTabItem(
    title = HTML(paste( options$longLabel, settingsUI(ns('settings')) ))
)
```

### appStep-specific structure of a module server function

The following template shows how to declare an appStep server function 
by extending the Shiny module function declaration with additional 
arguments.

```r
# <appStep>/<appStep>_server.R
appStepServer <- function(id, options, bookmark, locks) {
    moduleServer(id, function(input, output, session) {
    # ...
    # bookmark handling
    # module return value as a list
})}
```

where the required arguments in addition to the standard instance id are:

- **options** = the assembled step options as defined in module.yml and config.yml
- **bookmark** = a reactive carrying the contents of an incoming bookmark file
- **locks** = documentation pending (mainly used by MDI standardized app steps)

Additional important contents of the appStep module server 
are discussed next.
