---
title: Code Viewers
parent: Developer Tools
has_children: false
nav_order: 60
---

## {{page.title}}

A concept of the MDI apps framework is to allow app code to be viewed and updated in the app
while it is running. One part of this are code viewers/editors
that you can attach to a page or widget. 

Top-level code viewing is always provided by the framework in the page header,
but you will find it useful to attach viewers
to specific UI objects to give rapid access to context-relevant code. 

A code viewer is the same widget as a code editor, the only
difference is whether it is editable or read-only. Read-only 
code viewing gives all users the fastest possible understanding 
what your code actually does.

The MDI code viewer/editor is based on the well-established
[Ace Code Editor](https://ace.c9.io/) used in many well known platforms.

### Create a code viewer UI link

Code viewers can be launched from any server function as needed, but the typical
usage is to create a link within your module's UI function:

```r
# myModule_ui.R
aceEditorLink(id = ns('aceEditor'), class = "my-class")
```

that shows the Ace code editor via your module's server
function with the icon is clicked:

```r
# myModule_server.R
observeEvent(input$aceEditor, {
    showAceEditor(
        session,
        ... # see docs below
    )  
})
```

### Using the showAceEditor load function

The editor itself appears in a stateful modal dialog,
where 'stateful' means that the editor will typically return
to the same state when the same icon is clicked a second time.

As a dialog element, the code viewer does not follow the typical
UI+Server pattern, instead, it is launched via the `showAceEditor()` function:

```r
# aceEditor_utilities.R
showAceEditor <- function(
    session,
    baseDirs = NULL,
    showFile = NULL,
    editable = FALSE,
    loaded = NULL, 
    tall = FALSE,
    wide = FALSE,
    sourceError = NULL,
    sourceErrorType = ""
)
```

where:
- **session** = the session object of the calling server function
- **baseDirs** = one or more directories from which all files are shown as trees
- **showFile** = a single target file to show in lieu of baseDirs
- **editable** = whether to allow users to edit the files they open
- **loaded** = a list of files that have been previously opened in this R session
- **tall** = whether the dialog is currently extra tall
- **wide** = whether the dialog is currently extra wide
- **sourceError** = restricted for apps framework use
- **sourceErrorType** = restricted for apps framework use

Thus, you can show one or more directories, or a single file.

The following is a common value for `editable` which makes
file contents read-only in normal use, but editable when the 
`developer` flag is set:

```r
showAceEditor(
    ...,
    editable = serverEnv$IS_DEVELOPER
)  
```

### Additional references

For complete details, see:

- [mdi-apps-framework : Ace code editor](https://github.com/MiDataInt/mdi-apps-framework/tree/main/shiny/shared/session/modules/widgets/framework/aceEditor)
