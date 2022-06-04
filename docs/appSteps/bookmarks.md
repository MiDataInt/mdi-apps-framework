---
title: Bookmark Handling
parent: App Steps
has_children: false
nav_order: 50
---

## {{page.title}}

An essential feature offered by the MDI apps framework is
sophisticated bookmarking that is more versatile and user-friendly
than the approaches used by R Shiny, where an **MDI bookmark** is 
a JSON-formatted text file carrying comprehensive information
on the state of an app to allow it to be reloaded seamlessly.

Bookmarking is implemented
at the level of the appStep, i.e., each appStep module is responsible
for declaring and acting on its required bookmark contents. This is accomplished
by including save and load instructions at the end of an appStep module.

### Specifying appStep values to save in a bookmark

Three optional elements of an appStep
return list are always saved to bookmarks, in addition to being
available for use in the app:

- **input** = the current state of inputs on the appStep page
- **settings** = the current state of inputs under the appStep-level settings icon
- **outcomes** = a reactive, or a list of reactives, that return(s) values derived from user actions

```r
# <appStep>/<appStep>_server.R
appStepServer <- function(id, options, bookmark, locks) {
    moduleServer(id, function(input, output, session) {
    # ...
    list( # the module's return value
        input = input,
        settings = settings$all_,
        outcomes = reactive({ list(
            <outcomeName> = ...
        ) })
        # outcomes = list(  # alternative use
        #     <outcomeName> = reactive(...)
        # )
    )
})}
```

Importantly, ONLY **input**, **settings**, and **outcomes** values are stored in 
bookmarks. All other named objects in the module return value list are
considered to be for app internal use only. If you need to save something 
that is not an input or a setting, be sure to declare it as an outcome.

### Restoring appStep state from an incoming bookmark

When a user loads a bookmark from the framework launch page, 
that information is passed to all appStep modules via the bookmark reactive.
The following code illustrates how to handle the incoming bookmark's contents.

```r
# <appStep>/<appStep>_server.R
appStepServer <- function(id, options, bookmark, locks) {
    moduleServer(id, function(input, output, session) {
    # ...
    observe({
        bm <- getModuleBookmark(id, module, bookmark, locks)
        req(bm)
        settings$replace(bm$settings)
        if(!is.null(bm$outcomes)) {
            # reload outcomes as needed
        }
    })
})}
```

By convention and for clarity, always place the bookmark observer 
in the penultimate position of the appStep server function, 
just above the module return value that declares the 
input, settings, and outcomes that can be found in the bookmark.

### Auto-saved bookmarks and page reloading

In addition to user-saved bookmarks, the apps framework automatically
creates a bookmark every time a session ends, e.g., when
the user closes or reloads the web browser page, which
supports two additional features.

- quick launch to the most recent state using the "auto saved" entry on the launch page
- page reloading by clicking the top-left page label ("MDI" by default)