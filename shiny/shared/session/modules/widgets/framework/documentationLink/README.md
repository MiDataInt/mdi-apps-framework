---
title: Documentation Link
parent: User Feedback
has_children: false
nav_order: 40
---

## {{page.title}}

It is very useful to have links to the appropriate complete
documentation for your app within the app itself.
The `documentationLink` widget puts such a link on your page.

## documentationLinkUI arguments

The `documentationLinkUI` function takes a single argument in addition to 'id':

```r
# documentationLink_ui.R
documentationLinkUI <- function(id, isHeader = TRUE)
```

where setting `isHeader` to TRUE reduces the icon size in a manner
appropriate for inclusion in a step header or box title,
which is the most appropriate placement in many widgets.

For example:

```r
# module_ui.R
documentationLinkUI(ns(id))
```

## documentationLinkServer arguments

The `documentationLinkServer` function takes various arguments in addition to 'id':

```r
# documentationLink_server.R
documentationLinkServer <- function(
    id, 
    gitUser = "MiDataInt",
    repository = NULL,
    docPath,
    anchor = NULL
)
```

where:

- **id** = the id of the settings widget
- **gitUser** = the GitHub user or organization
- **repository** = the base repository name, if not "midataint.github.io"
- **docPath** = the relative file path to the documentation target, e.g., "path/to/docs.html" (".html" is optional)
- **anchor** = the name of an optional heading anchor on the page, e.g., "first-heading"

For example:

```r
# module_server.R
documentationLinkServer(id, "wilsontelab", "svx-mdi-tools", "path/to/README.html")
```

### Step-level documentation

Many appStep modules have a dedicated documentation page.
First, activate the documentation icon link in _ui.R_:

```r
# <appStep>/<appStep>_ui.R
<appStep>UI <- function(id, options) {
    ns <- NS(id)    
    standardSequentialTabItem(
        HTML(paste( options$longLabel, documentationLinkUI(ns('documentation')) ))
        # etc.
    )
}
```

Then activate the documentation server in _server.R_:

```r
# <appStep>/<appStep>_server.R
<appStep>Server <- function(id, options, bookmark, locks) {
    moduleServer(id, function(input, output, session) {
    documentationLinkServer('documentation',  ...)
    # etc.
})}
```
