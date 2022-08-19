---
title: Header Links
parent: App Steps
has_children: false
nav_order: 70
---

## {{page.title}}

As described in
[Developer Tools]({{ "/docs/developer-tools/00_developer-tools" | relative_url }}),
the apps framework provides several useful dialogs for actions
such as code viewing, testing, and documentation access, which are typically placed
into the header of either an appStep page or a widget box.

`mdiHeaderLinks` is a convenience
wrapper that makes it easy to attach the required support links
to an app module header in the standard format.

Other frameworks modules further wrap `mdiHeaderLinks`
as described below.

### mdiHeaderLinks UI function

To include a set of icon links, call `mdiHeaderLinks()`
in your module's UI function:

```r
# mdiHeaderLinks.R
mdiHeaderLinks <- function(
    id = NULL,  
    documentation = FALSE,
    terminal = FALSE,
    console = FALSE,
    code = FALSE,
    settings = FALSE
)
```

```r
# myModule_ui.R
myModuleUI <- function(id) {
    tagList(
        mdiHeaderLinks(id, documentation = TRUE) # etc. 
    )
}
```

where:

- **id** = id of the calling module
- **documentation** = whether to include a context-specific documentation link
- **terminal** = whether to include a link to a context-specific terminal emulator
- **console** = whether to include a link to a context-specific R console
- **code** = whether to include a link to a context-specific code viewer/editor
- **settings** = whether to include a link to a context-specific settings panel

Simply set the required links values to `TRUE`.

Notice that mdiHeaderLinks is a wrapper around other modules,
it is not a module itself, so pass it the raw `id` of the calling
module. Do _not_ use `mdiHeaderLinks(ns(id))`.

### activateMdiHeaderLinks server function

Activate the header links from within your server function
by calling `activateMdiHeaderLinks()` with context values that match
the specific links you included in your UI:

```r
# mdiHeaderLinks.R
activateMdiHeaderLinks <- function(
    id,
    session,
    url = NULL,
    dir = NULL,
    envir = NULL,
    baseDirs = NULL,
    settings = FALSE,
    ...
)
```

```r
# myModule_server.R
myModuleServer <- function(id) { 
    moduleServer(id, function(input, output, session) {
        module <- 'myModule'
        settings <- activateMdiHeaderLinks(
            id,
            session,
            url = getDocumentationUrl("path/to/docs/README", domain = "xxx"),
            dir = getAppStepDir(module),
            envir = environment(),
            baseDirs = getAppStepDir(module),
            settings = TRUE
        )
})}
```

where:

- **id** = id of the calling module
- **session** = session object of the calling module
- **url** = context-specific documentation url (matches UI `documentation`)
- **dir** = context-specific directory for the terminal emulator (matches UI `terminal`)
- **envir** = context-specific R environment for the R console (matches UI `console`)
- **baseDirs** = context-specific vector of directories for the code viewer/editor (matches UI `code`)
- **settings** = whether to activate the settings server using (matches UI `settings`)
- **...** = additional arguments passed to [settingsServer]({{ "/docs/settings/arguments" | relative_url }})

The example above shows typical values for `activateMdiHeaderLinks()`
that use functions `getDocumentationUrl()`, `getAppStepDir()`, and `environment()`,
which print the links to the current module's paths and R environment.

The return value of `activateMdiHeaderLinks()` is the object returned by
`settingsServer()`, so you should assign it to `settings` as illustrated above. 

### Using mdiHeaderLinks in a appStep module

It is recommended to include header links at the top of your appStep pages.
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
    terminal = FALSE,
    console = FALSE,
    code = FALSE,
    settings = FALSE
)
```

```r
# myAppStepModule_ui.R
myAppStepModuleUI <- function(id) {
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

where arguments from `id` and below are the same as described above
for `mdiHeaderLinks()`, which is called by `standardSequentialTabItem()`.

There is no dedicated sequential tab item server function - 
simply call `activateMdiHeaderLinks()` in your appStep module as
described above. 
