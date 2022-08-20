---
title: Header Links
parent: App Steps
has_children: false
nav_order: 70
---

## {{page.title}}

As described in
[Developer Tools]({{ "/docs/developer-tools/00_developer-tools" | relative_url }}),
[Settings Panels]({{ "/docs/settings/00_settings" | relative_url }}),
and elsewhere, the apps framework provides several useful links and dialogs for actions
such as documentation, code viewing, R consoles, and option setting.
These links are best placed into the header of either an appStep page or a widget box
using the `mdiHeaderLinks` interface.

### mdiHeaderLinks usage overview

At the heart of `mdiHeaderLinks` are the `mdiHeaderLinks()` and
`activateMdiHeaderLinks()` functions, used in a module's UI and server functions,
respectively. However, at most points in your app `mdiHeaderLinks()`
will be called on your behalf by MDI appStep and box wrapper functions. 
You will typically call `activateMdiHeaderLinks()` yourself 
within a module's server function.

Below we first describe the two main `mdiHeaderLinks` functions to enumerate
the options offered by the interface and then show how those options
are passed to appStep and box wrapper functions.

### mdiHeaderLinks UI function

To include a set of icon links, call `mdiHeaderLinks()` in a module's UI function:

```r
# mdiHeaderLinks.R
mdiHeaderLinks <- function(
    id = NULL,  
    type = c("box", "appStep", "none"),
    documentation = FALSE,
    reload = FALSE,
    code = FALSE,
    console = FALSE,    
    terminal = FALSE,
    download = FALSE,
    settings = FALSE
)
```

```r
# myModule_ui.R
myModuleUI <- function(id) {
    tagList(
        mdiHeaderLinks(id, type = "box", documentation = TRUE) # etc. 
    )
}
```

where:

- **id** = id of the calling module
- **type** = the kind of header into which icons are being place
- **documentation** = whether to include a context-specific documentation link
- **reload** = whether to include a link to reload/refresh/sync the module
- **code** = whether to include a link to open a context-specific code viewer/editor    
- **console** = whether to include a link to a context-specific R console
- **terminal** = whether to include a link to a context-specific terminal emulator
- **download** = whether to include a link to download the module contents
- **settings** = whether to include a link to a context-specific settings panel

Simply specify the header type and set the required link values to `TRUE`.

Notice that mdiHeaderLinks is a wrapper around other modules,
it is not a module itself, so pass it the raw `id` of the calling
module, i.e., `mdiHeaderLinks(id)`, not `ns(id)`.

### activateMdiHeaderLinks server function

Activate the header links from within your server function
by calling `activateMdiHeaderLinks()` with context values that match
the specific links you included in your UI:

```r
# mdiHeaderLinks.R
activateMdiHeaderLinks <- function(
    session,
    ...,
    url = NULL,
    reload = NULL,
    baseDirs = NULL,
    envir = NULL,
    dir = NULL,
    download = NULL,
    settings = NULL
)
```

```r
# myModule_server.R
myModuleServer <- function(id) { 
    moduleServer(id, function(input, output, session) {
        module <- 'myModule'
        settings <- activateMdiHeaderLinks(
            session,
            url = getDocumentationUrl("path/to/docs/README", domain = "xxx"),
            dir = getAppStepDir(module),
            envir = environment(),
            baseDirs = getAppStepDir(module),
            settings = id,
            immediate = TRUE # plus any other arguments passed to settingsServer()
        )
})}
```

where:

- **session** = session object of the calling module
- **...** = additional arguments passed to [settingsServer]({{ "/docs/settings/arguments" | relative_url }})
- **url** = context-specific documentation url (matches UI `documentation`)
- **reload** = callback function with no arguments to handle the reload action (matches UI `reload`)
- **baseDirs** = context-specific vector of directories for the code viewer/editor (matches UI `code`)
- **envir** = context-specific R environment for the R console (matches UI `console`)
- **dir** = context-specific directory for the terminal emulator (matches UI `terminal`)
- **download** = download handler for the download link, `created with shiny::downloadHandler()` (matches UI `console`)
- **settings** = the value passed as parentId to `settingsServer()` (matches UI `settings`)

The example above shows typical values for `activateMdiHeaderLinks()`
that use functions `getDocumentationUrl()`, `getAppStepDir()`, and `environment()`
to create links to the current module's paths and R environment.

`activateMdiHeaderLinks()` returns the same object returned by
`settingsServer()`, so you should assign it to `settings` as illustrated. 

### Using mdiHeaderLinks in a appStep module

We recommend including header links at the top of many appStep pages.
`standardSequentialTabItem()` wraps `mdiHeaderLinks()` to
place the links with the standard location and formatting:

```r
# sequentialMenu.R
standardSequentialTabItem <- function(
    pageTitle,
    leaderText,
    ..., 
    id = NULL,  
    documentation = FALSE,
    reload = FALSE,
    code = FALSE,
    console = FALSE,    
    terminal = FALSE,
    download = FALSE,
    settings = FALSE
)
```

```r
# myAppStepModule_ui.R
myAppStepModuleUI <- function(id, options) {
    standardSequentialTabItem(
        options$longLabel,
        options$leaderText, 
        id = id,
        documentation = TRUE,
        terminal = TRUE,
        console = TRUE,
        code = TRUE,
        settings = TRUE,
        # appStep UI elements go here
    )    
}
```

where arguments from `id` and below are the same as for `mdiHeaderLinks()`.

There is no dedicated appStep server function - 
simply call `activateMdiHeaderLinks()` in your appStep module as
described above. 

### Using mdiHeaderLinks in a widget box module

We recommend that data display widgets be placed inside
`shinydashboard::box()` panels, which allows `mdiHeaderLinks`
to be placed in an intuitive location immediately after the 
panel's title. 
Some MDI widget boxes handle `mdiHeaderLinks` in a dedicated
fashion (see below), or you can use the
`mdiBox` wrapper around `shinydashboard::box()` in your module's UI:

```r
# mdiHeaderLinks.R
mdiBox <- function(
    id, 
    title,
    ...,
    documentation = FALSE,
    reload = FALSE,
    code = FALSE,
    console = FALSE,    
    terminal = FALSE,
    download = FALSE,
    settings = FALSE
)
```

```r
# myWidgetModule_ui.R
myWidgetModuleUI <- function(id) {
    ns <- NS(id)
    mdiBox(id, "My Title", settings = TRUE, textOutput(ns('text'))) # etc.
}
```

where:

- **title** = the box title
- **...** = arguments and UI elements passed to `shinydashboard::box()`
- all other arguments are the same as for `mdiHeaderLinks()`.

There is no dedicated mdiBox server function - 
simply call `activateMdiHeaderLinks()` in your widget module as
described above. 

Please note that `mdiBox()` is intended for use once within a box widget module.
It probably will not work as you expect if you try to use it multiple times
or within an appStep module as it does not create a new namespace.

### MDI widgets with dedicated mdiHeaderLinks handling

Other standard MDI data display widgets offer their own specific
handling of `mdiHeaderLinks` that you can extend for
your own needs, such as `staticPlotBox` and others. 
Please follow the documentation of those widgets.
