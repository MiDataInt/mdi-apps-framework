---
title: R Console
parent: Developer Tools
has_children: false
nav_order: 80
---

## {{page.title}}

A concept of the MDI apps framework is to allow an app to be developed in the app
while it is running. One part of this is an R console
that you can attach to a page or widget. 

The console opens in a dialog and allows you to execute
R commands within the exact environment of the calling module.
For example, a plot box might offer an R console
to allow you to try out a new plotting function where you know
you will have access to all relevant data objects.

Although the R console is mostly targeted to developers,
it is available to all users in local and remote modes.
It is disabled on public servers for obvious reasons.

> Do not confuse the R Console with the Command Terminal.
The former executes commands in R, the latter executes commands
on the host operating system.

### Create an R console UI link

R consoles can be launched from any server function as needed, but the typical
usage is to create a link within your module's UI function:

```r
# myModule_ui.R
if(!serverEnv$IS_SERVER) rConsoleLink(id = ns('console'), class = "my-class")
else ""
```

that shows the R console via your module's server
function with the icon is clicked:

```r
# myModule_server.R
if(!serverEnv$IS_SERVER) observeEvent(input$console, {
    req(!serverEnv$IS_SERVER)
    showRConsole(
        session,
        ... # see docs below
    )  
})
```

Note that it is appropriate to reinforce the suppression
of R consoles on public servers, although the underlying module
will only load in local and remote modes.

### Using the showRConsole load function

The console itself appears in a stateful modal dialog,
where 'stateful' means that the console will reload the same code
when the same icon is clicked a second time.

As a dialog element, the R console does not follow the typical
UI+Server pattern, instead, it is launched via the `showRConsole()` 
function:

```r
# rConsole_utilities.R
showRConsole <- function(
    session,
    envir = NULL,
    label = NULL,
    tall = FALSE, 
    wide = FALSE
)
```

where:
- **session** = the session object of the calling server function
- **envir** = the environment in which R code is to be evaluated
- **label** = text appended to "R Console" in the dialog header
- **tall** = whether the dialog is currently extra tall
- **wide** = whether the dialog is currently extra wide

The `envir` argument is usually not needed - it defaults to the
environment of the function from which `showRConsole()` was called.

### Additional references

For complete details, see:

- [mdi-apps-framework : R console](https://github.com/MiDataInt/mdi-apps-framework/tree/main/shiny/shared/session/modules/widgets/framework/rConsole)
