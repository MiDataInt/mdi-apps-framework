---
title: Code Utilities
parent: Developer Tools
has_children: false
nav_order: 20
---

## {{page.title}}

The MDI apps framework has a series of utility functions
that are always available to any app, unless overwritten
by a declaration in your app code, which is always sourced
last when an app is loaded.

MDI utility functions are too numerous to document individually, 
but if you find that base R
lacks some obvious function, consider perusing the scripts
identified below to see if it exists there.

The utility folder scripts are organized by overall purpose
with descriptive names, such as _strings.R_ and _stats.R_, 
and each script contains typically multiple support functions.
Read the comments and code for usage information.

### Global utilities

The following framework folder:

- [mdi-apps-framework : global utilities](https://github.com/MiDataInt/mdi-apps-framework/tree/main/shiny/shared/global/utilities)

has a series of R scripts that are sourced whenever an app is 
loaded. Importantly, they are sourced outside the scope of a specific
user session, so do not have access to session variables,
such as `input` or `session`, unless they are passed as arguments
to the function. 

Thus, these scripts are where you will find
generic utilities such as `commify()` - to format numbers
with commas - `fitTrendline()` - a wrapper around various
curve-fitting functions -  etc.

### Session utilities

The following framework folder:

- [mdi-apps-framework : session utilities](https://github.com/MiDataInt/mdi-apps-framework/tree/main/shiny/shared/session/utilities)

has a series of R scripts that are also sourced whenever an app is 
loaded, however, these are sourced within the scope of a specific
user session and have access to all session variables
without passing them as arguments. 

Please note that many scripts in the session utilities
folder are used internally by the framework to construct
appStep pages and similar, and are less likely 
to have simple utility functions - see the global utilities, above.
However, advanced developers may find it necessary to call 
these functions when writing complex pages.

### Module utilities

In addition to global and session utilities provided by and
for the framework overall, many modules have their own 
utilities scripts, named _\<moduleName\>_utilities.R_
by default. Please check a module's folder for details.

Many module-level utilities are described in these
documentation pages for modules that are part of the MDI framework.

### Developer code monitoring tools

A final set of utilities help developers monitor code for
development and debugging purposes. Many are found here:

- [mdi-apps-framework : developer utilities](https://github.com/MiDataInt/mdi-apps-framework/blob/main/shiny/shared/developer/utilities/developer.R)

That script is only sourced in developer mode and its functions
must not be used in production apps.

### External utility libraries

If you have a utilities library you particularly like,
remember that you can always 
[require an R data package](/mdi-apps-framework/docs/developer-tools/r-package-dependencies.html)
for your tool suite or app. You are not required 
to use the MDI's utility functions.
