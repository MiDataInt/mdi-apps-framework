---
title: Object Caching
has_children: true
nav_order: 70
---

## {{page.title}}

Some analyses executed by your app will be substantially
slower than others. When a slow process creates a 
data object that is likely to be needed again in the future,
it is best to cache, i.e., save
that object for rapid reloading on subsequent calls, either
in the same or future app sessions.

The MDI apps framework provides two interfaces
to facilitate object caching, which differ in
the level at which they are implemented. 

You can also use other standard caching tools like
[R Shiny bindCache](https://shiny.rstudio.com/app-stories/weather-lookup-caching.html)
or other packages like
[storr](https://cran.r-project.org/web/packages/storr/index.html).
