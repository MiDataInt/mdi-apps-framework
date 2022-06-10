---
title: Modal Dialogs
parent: User Feedback
has_children: false
nav_order: 10
---

## {{page.title}}

We encourage the use of **modal popup dialogs** to communicate
with users. The MDI apps framework provides a wrapper 
around the R Shiny `showModal` utility for a consistent experience
for the developer and user.

The wrapper is implemented in a single function called
`showUserDialog`. The following code block shows the basic call structure.

```r
# <moduleName>_server.R
showUserDialog(
    title, 
    ..., 
    callback = function(parentInput) NULL,
    size = "s", 
    type = 'okCancel', 
    footer = NULL, 
    easyClose = TRUE, 
    fade = NULL
)
```

where :

- **title** = the title shown at the top of the popup
- **...** = the UI elements and inputs used to populate the popup
- **callback** = a function called when the popup closes with an affirmative/OK response
- **size** = the size of the popup as s/m/l
- **type** = a setting that determines the actions buttons to show (see below)
- **footer** = the buttons to place in the footer if not using a default `type`
- **easyClose** = allow users to close the dialog by clicking outside of it
- **fade** = whether or not to use an entry animation when opening the dialog

Many of these options are of the same name as the parent Shiny `modalDialog` function.

The valid types are:

- **okOnly** = an "information only" dialog
- **okOnlyCallback** = an "information only" dialog with a callback
- **okCancel** = an action that requires input and/or confirmation
- **saveCancel** = to confirm a file save action
- **deleteCancel** = to confirm a file deletion
- **discardCancel** = to confirm a discard changes action
- **okOnlyWithAction** = redundant with okOnlyCallback (oops)

When the user closes the box with an "OK" or other confirm action,
the callback function is called, with a single input reactiveValues object 
passed to it that carries the values of any inputs in the dialog.
