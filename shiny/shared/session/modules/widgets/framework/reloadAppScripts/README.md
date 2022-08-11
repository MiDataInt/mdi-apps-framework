---
title: Dynamic Script Refresh
parent: Developer Tools
has_children: false
nav_order: 50
---

## {{page.title}}

A concept of the MDI apps framework is to allow app code to be viewed and updated in the app while it is running.

### Re-sourcing app scripts without page reload

One part of this is that a **refresh link** is placed 
in the top menu bar when working in single-user
developer modes, e.g. `mode = 'local', developer = TRUE`. 
Clicking the link allows a developer
to re-`source()` many of the session scripts used
by an app without reloading the web page.

{% include figure.html file="developer-tools/refresh-icon.png" width="250px" %}

Not all scripts can be re-loaded in this fashion.
A complete description is beyond the scope here,
but in general, appStep modules cannot be dynamically
updated, whereas utility scripts can.
Let trial and error be your guide. One suggestion
is to make liberal use of module utility scripts
that can be re-sourced.

This feature can save a lot of time reloading 
apps when adding app features, 
especially if they are doing slower work
on each reload.

### Page reload via auto-saved bookmarks

For scripts that cannot be dynamically refreshed,
you can click on the upper left page label
(default value "MDI") to force a hard reload
of the page where the auto-saved bookmark will
take you back to the same app step and state, but
now having reloaded the framework and all appStep modules.

This feature is always available in all apps
but is most useful to developers, which is
why it is not advertised to users in a more obvious way.
