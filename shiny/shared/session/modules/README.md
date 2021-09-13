
----------------------------------------------------------------
 Portal-specific Shiny modules for streamlined development
----------------------------------------------------------------

**modules** are R Shiny modules that define resuable UI components
and associated server logic for app steps, widgets, etc.

The concept is that one module defines one discrete, reusable part
of the UI that has both UI elements and code logic. Please read
about Shiny modules online to understand how they work.

By convention, modules following the naming convention

    module_server.R      the function that defines the module's code actions
    module_ui.R          the function that returns the UI elements for the module
    module_utilities.R   additional generic support scripts for the module

Like any R functions, module server functions can return a single
object that can be used by the calling code. Typical return values
are a list of some combination of reactive objects (to dynamically
modify the UI) and methods to be applied to the element or its data.

'appSteps' defines modules with specific return values that are
used both in code as well as to create bookmarks.

