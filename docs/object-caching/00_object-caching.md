---
title: Object Caching
has_children: true
nav_order: 70
---

## {{page.title}}

Some analyses executed by your app will be substantially
slower than others. When a slow process creates a 
data object that is likely be needed again in the future,
it is best to cache, i.e., save
that object for rapid reloading on subsequent calls, either
in the same or future app sessions.

The MDI apps framework provides two interfaces
to facilitate object caching, which differ mainly in
the level at which they are implemented. 


cache class
persistentCache


