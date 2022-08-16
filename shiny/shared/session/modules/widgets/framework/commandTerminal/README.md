---
title: Terminal Emulator
parent: Developer Tools
has_children: false
nav_order: 70
---

## {{page.title}}

Apps are great, but sometimes it is helpful 
to explore the MDI server from the command line.
In keeping with the "in-app" MDI experience, the apps
framework offers a simple terminal emulator, i.e., command terminal,
that can be attached to UI elements.

The emulator is based on the R `system` command and
is not meant to be a complete replacement for a 
proper, external command terminal when heavy developer use is needed.
The in-app terminal is best for "poke and peek" tasks.

Although the terminal emulator is mostly targeted to developers,
it is available to all users in local and remote modes.
It is disabled on public servers for obvious reasons.

> Do not confuse the R Console with the Command Terminal.
The former executes commands in R, the latter executes commands
on the host operating system.

### Create a terminal emulator UI link

Terminal emulators can be launched from any server function as needed, but the typical
usage is to create a link within your module's UI function:

```r
# myModule_ui.R
if(!serverEnv$IS_SERVER) commandTerminalLink(id = ns('terminal'), class = "my-class")
else ""
```

that shows the terminal emulator via your module's server
function with the icon is clicked:

```r
# myModule_server.R
if(!serverEnv$IS_SERVER) observeEvent(input$terminal, {
    req(!serverEnv$IS_SERVER)
    showCommandTerminal(
        session,
        ... # see docs below
    )  
})
```

Note that it is appropriate to reinforce the suppression
of terminals on public servers, although the underlying module
will only load in local and remote modes.

### Using the showCommandTerminal load function

The terminal itself appears in a stateful modal dialog,
where 'stateful' means that the terminal will return
to the same state when the same icon is clicked a second time.

As a dialog element, the terminal emulator does not follow the typical
UI+Server pattern, instead, it is launched via the `showCommandTerminal()` function:

```r
# commandTerminal_utilities.R
showCommandTerminal <- function(
    session, 
    host = NULL, 
    pipeline = NULL,
    action = NULL,
    runtime = NULL,
    dir = NULL, 
    forceDir = FALSE,
    tall = FALSE,
    wide = FALSE
)
```

where:
- **session** = the session object of the calling server function
- **host** = the host to ssh into when running terminal commands
- **pipeline** = as used in 'mdi <pipeline> shell --action <action> --runtime <runtime>'
- **action** = see pipeline
- **runtime** = see pipeline
- **dir** = the suggested directory in which to open the terminal
- **forceDir** = always use `dir`, even if there is a cached value
- **tall** = whether the dialog is currently extra tall
- **wide** = whether the dialog is currently extra wide

Typical usage for many apps would set only the `dir` argument,
the others are mainly for the Pipeline Runner app.

### Additional references

For complete details, see:

- [mdi-apps-framework : command terminal emulator](https://github.com/MiDataInt/mdi-apps-framework/tree/main/shiny/shared/session/modules/widgets/framework/commandTerminal)
