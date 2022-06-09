---
title: Configuration Editor
parent: Server Deployment
has_children: false
nav_order: 40
---

## {{page.title}}

A concept of the MDI apps framework
is to allow the server
and apps to be updated and developed in an app
while it is still running. 

One part of this is that users can update the configuration
of the local server in-app using the gear icon in the top
menu bar, which can be clicked to expose a file editor.

The feature is only active in single-user modes - you wouldn't
want users changing the configuration of a shared public server.

### Using the in-app configuration editor

Using this feature is easy and obvious:
- click the icon
- choose a file to edit
- make the required changes
- click Save

The framework will recognize the changes that 
were made and act accordingly to install any new
repositories or packages and re-launch the server
and/or app.
