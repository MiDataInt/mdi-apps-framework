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

The steps for your app are listed in its config.yml file,
following the instructions found in the template file and 
this example:

```yml
# <app>/config.yml
appSteps:
    <stepName>: # your name for the appStep
        module: <moduleName>    # expect moduleNameUI and moduleNameServer
        shortLabel: ...         # shown on dashboard menu
        shortDescription: ...   # shown on the Overview page
        options:
            longLabel: ...      # title for the appStep tabbed page
            alwaysVisible: false
            # module-specific options go here
    <stepName>:  # etc.
```

The config.yml template file has declarations for 
an initial app step sequence based on standardized modules
that is common to many apps, with support for
additional source file upload, sample assignments into groups, and
asynchronous data analysis.

### Step module default configuration in \<appStep\>/module.yml

In addition to declarations made in an app's config.yml file,
an appStep module can have standard declarations in its module.yml file.
These act as defaults that can be overridden by an app, in addition
to other declarations discussed below. 

```yml
# <appStep>/module.yml
shortLabel: ...
shortDescription: ...
longLabel: ...
```

Due to declarations in module.yml, many times the appStep declarations
in config.yml can be quite simple:

```yml
# <app>/config.yml
appSteps:
    stepName_1: 
        module: moduleName_1    
```

### Other declarations in \<appStep\>/module.yml

The other appStep-specific declarations in module.yml are
**types** and **sourceTypes** that establish step sequence,
as discussed in detail on a later page.

Other declarations permitted in an appStep module.yml are the same
as many component configuration files, including **packages**
and **settings**, discussed in detail on a later pages.

### appStep-specific structure of a module UI function

The following template shows how to declare an appStep UI function 
by:
- extending the Shiny module function declaration with additional 
arguments
- wrapping the appStep UI contents with the required `standardSequentialTabItem` function

```r
# <appStep>/<appStep>_ui.R
appStepUI <- function(id, options) {
    ns <- NS(id) 
    standardSequentialTabItem(
        title,       # title for the appStep page
        leaderText,  # regular text below the title, above the UI elements
        ...          # UI elements for the appStep's tabbed page
    )
}
```

where the arguments in addition to the standard module instance id are:

- **options** = the assembled options defined in config.yml and module.yml

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
})}
```

where the arguments in addition to the standard instance id are:

- **options** = the assembled options defined in config.yml and module.yml
- **bookmark** = a reactive carrying the contents of an incoming bookmark file
- **locks** = XXX

Additional important contents of the appStep module server 
are discussed on the next pages.

