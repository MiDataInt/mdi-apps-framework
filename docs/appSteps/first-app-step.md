---
title: First App Step
parent: App Steps
has_children: false
nav_order: 40
---

## {{page.title}}

We encourage most apps to use the standardized `sourceFileUpload`
appStep module as their first app step. It offers well-tested code
for users to upload multiple data-packages for:

- organizing their work
- renaming samples
- performing comparative data analyses

{% include figure.html file="app-steps/source-file-upload.png" border=true %}

However, some apps may wish to enforce the analysis of only
a single data package per encounter. Such apps should instead
write their own custom appStep module to be used as the first app step.

### Specific requirements of first appStep modules

A first appStep module follows the same structure
as any appStep module, but with the following important _additional_
members of its return value list:

```r
# <appStep>/<appStep>_server.R
appStepServer <- function(id, options, bookmark, locks) {
    moduleServer(id, function(input, output, session) {
    list( # the module's return value
        outcomes = list(
            analysisSetName = reactive(...) # used for default naming of bookmark files
        ),
        loadSourceFile = function(incomingFile, suppressUnlink) # data passed from the universal launch page
    )
})}
```

The `analysisSetName` outcome must be provided, typically from 
a user input, as it is used to construct standardized bookmark names.

The `loadSourceFile` function is required to load the incoming 
data package file.  Please use `str(incomingFile)` when writing
your appStep module to understand how the file metadata are communicated.
