---
title: Step Return Values
parent: App Steps
has_children: false
nav_order: 20
---

## {{page.title}}

Later steps in an app often need to use the data and outcomes 
from one or more prior steps. Passing data from a parent or ancestor
step to a child step is achieved by adding elements to the list of values 
returned by an appStep module's server function. 
These returned values also determine what will be stored in bookmarks.

```r
# <appStep>/<appStep>_server.R
appStepServer <- function(id, options, bookmark, locks) {
    moduleServer(id, function(input, output, session) {
    # ...
    list( # the module's return value
        # reactive objects that communicate step readiness    
        # reactive objects to save in bookmarks
        # any other reactive or static objects needed by the app
    )
})}
```

See the next sections for detailed discussions of sequential
step readiness and bookmarking.

All elements are optional; simply omit them if not used by your app.
If your appStep doesn't need bookmarking or sequencing (e.g., is a last step),
use `NULL` or an empty list as the return value.

### Accessing appStep return values by name

All app code has a list object in its scope
called `app` that carries the keyed return values for each appStep module
(in addition to information about the app itself).
The data are accessed as follows:

```r
# <scriptName>.R
stepName <- "abc123"
stepData <- app[[stepName]]
```

where `stepName` is the name found in \<app\>/config.yml.

### Accessing appStep return values by type

Because your code might not know the name of the appStep
it needs to query, but might know its type (as discussed later),
the framework provides helper functions for accessing app step data:

```r
# <scriptName>.R
stepType <- "abc123"

# get the name of a step from its type
stepName <- getAppStepNameByType(stepType)

# get the return value list of a step from its type
stepData <- getAppStepByType(stepType)

# get one element of an app step's return value list from its type
stepValue <- getStepReturnValueByType(stepType, "valueName")

# get an app step's settings and outcomes elements from its type
stepSettings <- getStepSettingsByType(stepType)
stepOutcomes <- getStepOutcomesByType(stepType)

```

where `stepType` is the type found in \<appStep\>/module.yml.

### Accessing appStep return values by index

Finally, it is also possible to retrieve an app step's data from
its numerical index across all app steps, although this is usually
only needed with framework code, not in app code:

```r
# <scriptName>.R
stepName <- names(app$config$appSteps)[i] 
stepData <- app[[stepName]]
```
which demonstrates that you can access the contents of file 
\<app\>/config.yml via R object `app$config`.
