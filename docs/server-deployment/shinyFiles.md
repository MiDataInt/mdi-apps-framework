---
title: shinyFiles
parent: Server Deployment
has_children: false
nav_order: 20
---

## {{page.title}}

The MDI apps framework supports two ways that users 
can load and save files, from the computer running the web 
browser (the client machine), using the R Shiny
`fileInput` and `downloadHandler` features, or from the computer running
the web server (the server machine), using the 
[R shinyFiles package](https://cran.r-project.org/web/packages/shinyFiles/index.html).

{% include figure.html file="server-deployment/shinyFiles.png" border=true width="600px" %}

This section describes the shinyFiles approach,
which is especially useful to give users access to 
files on a remote or public server without having to 
transfer them to a desktop or laptop.

Notice that in local mode the client and server run
on the same computers, so the two methods read the same
file system. It can still be useful to use 
`shinyFiles` since the `fileInput` has upload size limits.

### Naming supported file paths

All server file paths are inaccessible by default - 
you must specifically list the paths you wish to
expose in _stage2-apps.yml_, as follows:

```yml
# mdi/config/stage2-apps.yml
paths:
  <pathName>: <server file path>
```

Each path is given a name used in the config authorization
sections, and the server file path to which it maps.

You could also call shinyFiles functions directly
and provide the file paths in code, but that practice is discouraged
as your app would then be difficult to share with others.

### shinyFiles wrapper functions

The next sections show how to create inputs that 
activate server file access via shinyFiles. There are additional
modules that are primarily intended to be used internally, e.g.,
in the sourceFileUpload appStep module. Only the relevant
ui and server code is shown.

### serverChooseDir button

The following shows how to create an icon to choose a server directory
selected by the user:

```r
# <moduleName>_ui.R
serverChooseDirIconUI(id)
```

```r
# <moduleName>_server.R
serverChooseDirIconServer(
    id, 
    input, 
    session,
    default_type = NULL,
    chooseFn = function(dir) NULL
)
```

### serverSaveFile button or link

The following shows how to create either an icon or a button
to save a file to a user-selected server path:

```r
# <moduleName>_ui.R
serverSaveFileButtonUI(
    id, 
    label, 
    filename, 
    filetype, 
    buttonType = "success"
)
serverSaveFileLinkUI(
    id, 
    label, 
    filename, 
    filetype
)
```

```r
# <moduleName>_server.R
serverSaveFileButtonServer(
    id, 
    input, 
    session, 
    filetype,
    default_type = NULL,
    saveFn = function(file) NULL
)
```

Notice that `serverSaveFileButtonServer()` is the same server function to use whether
your UI used a link or a button.

### Additional references

For details on the wrapper function code, see:

[mdi-apps-framework : shinyFiles](https://github.com/MiDataInt/mdi-apps-framework/blob/main/shiny/shared/session/utilities/shinyFiles.R)
