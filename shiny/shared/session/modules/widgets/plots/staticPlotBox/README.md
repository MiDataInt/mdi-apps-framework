---
title: staticPlotBox
parent: Plots
grand_parent: Display Widgets
has_children: false
nav_order: 10
---

## {{page.title}}

The **staticPlotBox** plot widget displays non-interactive plots that 
are optimized for the rapid, controlled generation of publication
ready images. It allows users to explore different display
properties like plot size, point size, legend placement, and so on,
in order to create and save predictable image files.

On screen, plots fill a single shinydashboard `box()`. The sizes of text
and other elements will not necessarily reflect the final image
but are nearly always legible and usable during data exploration.

A link allows for immediate download of the rendered plot,
with no guessing as to what it will look like in the png file.

The obvious drawback is that users cannot
interact directly with the data. If this is important for your app,
you should try the `interactivePlot` family of widgets. We have
ensured that the same colors are used in both widgets for 
a consistent final appearance. 
