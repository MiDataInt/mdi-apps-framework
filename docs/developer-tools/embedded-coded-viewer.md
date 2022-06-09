---
title: Embedded Code Viewers
parent: Developer Tools
has_children: false
nav_order: 60
---

## {{page.title}}

A concept of the MDI apps framework is to allow the framework
and app code to be viewed and updated in an app
while it is running. One part of this are code viewers and editors
that you can attach to a page or widget.

You can also make these features available in your
production app in read-only mode, if you wish to allow
users to see the code of an app or supporting pipeline
to know exactly what was done to generate data displays.

TODO: enable Edit+Save when in developer mode for these modules,
with auto-resourcing on file save.

### Code viewer for a specific file

The first code viewer provides access to a specific file via
a popup modal (only module-specific code is shown).

```r
# <moduleName>_ui.R
codeViewerModalUI(id)
```

```r
# <moduleName>_server.R
codeViewerModalServer( # the module has no return value
    id, 
    parentId,
    codeFile
)
```

where:
- **id** = the id of the widget
- **parentId** = the id of the module loading the widget
- **codeFile** = a string, reactive, or function with no arguments that returns a code file path

### General purpose code viewer

The more extensive code viewer provides an embedded
panel with a file tree and viewer/editor pane
 (only module-specific code is shown).

```r
# <moduleName>_ui.R
codeViewerUI(id)
```

```r
# <moduleName>_server.R
codeViewerServer( # the module has no return value
    id, 
    parentId,
    showApp = TRUE
)
```

where:
- **id** = the id of the widget
- **parentId** = the id of the module loading the widget
- **showApp** = if TRUE, also show root-level app scripts

### Additional references

For complete details, see:

- [mdi-apps-framework : code viewers](https://github.com/MiDataInt/mdi-apps-framework/tree/main/shiny/shared/session/modules/widgets/framework/codeViewer)

Also, the following demo app uses both code viewer modules as 
a working example:
- [mdi-demo-app](https://github.com/MiDataInt/demo-mdi-tools/tree/main/shiny/apps/demo)
