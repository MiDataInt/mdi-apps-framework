---
title: Asynchronous Execution
has_children: false
nav_order: 80
---

## {{page.title}}

Some analyses executed by your app will be substantially
slower than others. Very slow processes
can cause a significant performance degradation
because R, and thus your Shiny web server, are 
single-threaded. This means that when your server
is performing a slow process, no other requests
can be fulfilled and pages will appear to
hang. This is especially problematic for multi-user
servers, but even for single-user servers, e.g.,
when running on your desktop, it is best to let the
user continue working while the slow process is executed
in another R process.

Deferring a task to another R process and capturing the result later
is called **asynchronous execution**. It is achieved in R
using the
[future]()
and
[promises]()
packages. The MDI apps framework helps you use these 
packages to execute tasks asynchronously. 

TODO: further documentation pending the integration
of these code structure with framework v1.0.
