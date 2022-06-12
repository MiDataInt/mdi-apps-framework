---
title: Settings Panels
has_children: true
nav_order: 40
---

## {{page.title}}

The MDI apps framework encourages (but never demands) an uncluttered
UI for your app. In part, this means not placing dozens 
of option inputs onto the main UI page. We encourage you to expose
inputs to users as they need them in an organized fashion.

We help achieve this through a YAML-based
"settings" module, whose resulting gear/cog icon can be attached to 
an appStep page or to any box or widget in your UI. 

{% include figure.html file="settings/settings-icon.png" border=true width="400px" %}

When clicked, the icon opens a modal popup revealing structured option inputs.
Users quickly become accustomed to looking under that icon for context-relevant options.

{% include figure.html file="settings/settings-dialog.png" border=true width="400px" %}

Disadvantages are a small delay to being
able to edit options and that the values in force are not always
evident on screen. The few inputs where these concerns become large,
e.g., frequently modified, critically important user choices, should 
be extracted from settings and shown as one of the few on-page inputs.
