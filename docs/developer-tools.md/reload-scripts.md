---
title: Dynamic Script Refresh
parent: Developer Tools
has_children: false
nav_order: 40
---

## {{page.title}}

A concept of the MDI apps framework
is to allow the server
and apps to be updated and developed in an app
while it is still running. 

One part of this is that a 'refresh' link is placed 
in the top menu bar when working in single-user
developer modes. Clicking that link allows a developer
to re-source many of the session scripts in use
by an app without reloading the web page.

Not all scripts can be re-loaded in this fashion.
A complete description is beyond the scope here,
but in general, appStep modules cannot be dynamically
updated, whereas most other utility scripts can.
Let trial and error be your guide. One suggestion
is to make liberal use of module utility scripts
that will be properly re-sourced.

This feature can save a lot of time reloading 
apps, especially if they are doing slower work
on each reload.

For scripts that cannot be dynamically refreshed,
you can always click on the upper left page label
(with default value "MDI") to force a hard reload
of the page in which the auto-saved bookmark will
take you back to the same app step and state.
