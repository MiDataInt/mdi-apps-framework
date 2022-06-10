---
title: Constants
parent: Developer Tools
has_children: false
nav_order: 40
---

## {{page.title}}

In addition to code utilities, the MDI defines
a series of constants that establish common
framework behaviors and styles. Many are for
internal use, whereas others may be useful in your apps.

The constants defined by the framework are found here:

- [mdi-apps-framework : constants](https://github.com/MiDataInt/mdi-apps-framework/blob/main/shiny/shared/global/constants.R)

To make it obvious that these values are constants
that should not be changed, the object has the uppercase name
`CONSTANTS`.

### App-specific constants

You are free to add to the `CONSTANTS` list at any place 
in your app's code, e.g., in _server.R_. Just be sure
to always _add_ to the list, never replacing either the CONSTANTS
list or any of its named elements - doing so will break the framework.

```r
# GOOD
CONSTANTS$myValue <- 123

# VERY BAD
CONSTANTS <- list()
CONSTANTS$fileSuffixes <- list()
```

### Consistent plot colors

The framework-defined constant most useful to app developers
is `plotlyColors`, which allows you to access the color
values used by the plotly package by simple color name, for a consistent
appearance of plots:

```r
# <scriptName>.R
plot( 
    ...,
    col = CONSTANTS$plotlyColors$blue
)
```
