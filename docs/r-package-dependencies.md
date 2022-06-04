---
title: R Package Dependencies
has_children: false
nav_order: 10
---

## {{page.title}}

The MDI apps framework by default loads and attaches a substantial
set of commonly used R packages, listed here:

- [mdi-apps-framework packages.yml](https://github.com/MiDataInt/mdi-apps-framework/blob/main/shiny/shared/global/packages/packages.yml)

### App-specific R package declarations

All Stage 2 configuration files can also declare additional R package dependencies
required for the app to function, as follows:

```yml
# various config.yml or module.yml files
packages:
    R:  
        - <packageName>
        # etc.
    Bioconductor: null # or a list of packages
```

It is not harmful to declare R packages already listed elsewhere, 
they will only be installed once.

### Installing vs. attaching

R packages listed in the the mdi-apps-framework package.yml file are attached and 
their functions can be called directly, e.g., Shiny's <code>observeEvent()</code>.

In contrast, packages declared in an app are _installed_ but are never _attached_ 
using the <code>library()</code> function. Thus, you must either:

- call functions in full syntax, e.g., `package::function()` (**preferred**)
- call `library(package)` yourself (**discouraged**, you could break the framework)
