---
title: Progress Spinner
parent: User Feedback
has_children: false
nav_order: 50
---

## {{page.title}}

We try to keep motion in the MDI
apps framework to a minimum, but a few animations are
helpful for letting the user know the app has not hung 
and become unresponsive.

The page header has a small but nevertheless
visible and important progress "spinner" that you can
activate for tasks anticipated to take more than
a hundred milliseconds or so.

Long running tasks are best be 
[performed asyncrhonously](https://midataint.github.io/mdi-apps-framework/docs/asynchronous.html). Thus, the
spinner is appropriate for low to moderate task times. 

### Start, stop, and update the spinner

Three functions control the spinner's
visibility and, if desired, it's associated state message:

```r
# shiny/shared/global/utilities/ui.R
startSpinner <- function(session, caller = NULL, message = NULL)
updateSpinnerMessage <- function(session, message)
stopSpinner <- function(session, caller = NULL)
```
where:
- **session** = the session object of the calling server environment
- **caller** = optionally, the name of the calling function, for the server log stream
- **message** = optionally, a short text message to show in the page header next to the spinner

Using the spinner is straightforward, e.g.:

```r
# myModule_server.R
startSpinner(session)
for(i in 1:100) updateSpinnerMessage(session, message = i)
stopSpinner(session)
```

For debugging, spinner start and stop events are written to the server log stream.
