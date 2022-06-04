---
title: Passing Step Data
parent: App Steps
has_children: false
nav_order: 20
---

## {{page.title}}

It is often important to pass the data and outcomes from one app step 
to a subsequent step or steps, or for saving in bookmark files. 
This is achieved by adding elements to the list of values  
returned by an appStep module's server function.

```r
# <appStep>/<appStep>_server.R
appStepServer <- function(id, options, bookmark, locks) {
    moduleServer(id, function(input, output, session) {
    # ...
    list( # the module's return value

        # reactive objects that communicate step readiness    

        # reactive objects to save in bookmarks

        # any other reactive or static objects needed in the app
    )
})}
```

See the next sections for detailed discussions of sequential
step readiness and bookmarking.

All elements are optional; simply omit them if not used by your app.
If your appStep doesn't need bookmarking or sequencing (e.g. is a last step),
simply use `NULL` or an empty list as the return value.
