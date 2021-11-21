## R package dependencies

The **packages** folder provides:

- configuration declarations for R packages required to run the apps framework, or that are required by many apps (not packages peculiar to a specific app or analysis type, see below)
- functions to help load packages from an MDI installation's private library

These same files are also used by MDI::install() to discover the R packages
that are needed.

Developers should see comments in **packages.yml** to understand which packages
are attached to global environment (i.e, loaded using <code>library()</code>)
and whose functions can therefore be accessed directly in code. Packages that
are not attached can only be used in full syntax, e.g., <code>package::function()</code>.

### Packages private to an app or other component

Any app, module, analysis type, or other component can also declare its need
for a specific R package in either a **module.yml** or **config.yml** file,
by adding lines in this format:

```
packages: 
   R:  
    - xxx
    - yyy
   Bioconductor: null
```

Such packages are installed but never attached, so, similar to above, must be
used in full syntax, e.g., <code>package::function()</code>.

Please note that redundant declarations are not a problem, they will not be
installed twice, etc., so don't worry that you might be declaring a package 
a second time.
