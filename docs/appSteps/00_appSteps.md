---
title: App Steps
has_children: true
nav_order: 30
---

## {{page.title}}

The MDI Stage 2 Apps framework encourages
apps written in a stepwise structure corresponding to tabs
on the left sidebar. This reproducible organization 
makes it easy for users to understand how to use your app in
an organized fashion. 

App steps can be obligatorily sequential, but don't have to be.
Even for non-sequential activities, tab-based organization
is helpful and simplifying.

Many apps will use the same appStep module(s) for the first tab(s),
as these are designed for loading and structuring
data analyses without respect to the work being done. Later tabs
are typically specific to your app - much of your work
can focus on writing those modules, drawing on outcomes of the earlier steps.

You are free to write an app with a single step, i.e., to not use
step-based organization, in which case you should write a single appStep
module satisfying the requirements of a first (and only) app step.
