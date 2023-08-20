---
title: dataExplorer
parent: Standard Step Modules
grand_parent: App Steps
has_children: false
nav_order: 40
---

## {{page.title}} appStep module

The **dataExplorer** appStep is a highly generalized tool
that allows users to explore the data structures available
to an app by writing R code in the app to try out their own tables and plots.

The net effect is similar to an interface like RStudio, however, 
dataExplorer runs R code in the exact
context of your running app.  It is therefore useful
to developers wishing to explore what their app might
do before they commit to writing an appStep module. 

### Usage

The interface is self-explanatory. 
Write code in the code editors and click the buttons
to render the resulting tables and plots.

One valuable feature of dataExplorer is that it is a regular
appStep module, which means you can save bookmarks
that retain all code blocks you have been working on.
Thus, dataExplorer is often the first module we add to a new app. 
After working with it for a while, we have an idea of what appSteps 
should do. We then copy out the code, delete dataExplorer, and create those appSteps.

### Security alert

dataExplorer allows users
to execute arbitary R code on a computer or server.
As a result, it is always disabled on a public web server,
even if it is defined in your app.  

You should consider carefully whether to offer dataExplorer
even for use in local or remote modes, as inexperienced
users might still do harm with ill-advised commands,
even though they will be fully authorized to do so by
virtue of how they loaded the app. Accordingly, dataExplorer
is best considered a tool to support initial app development.
to be removed when an app matures.
