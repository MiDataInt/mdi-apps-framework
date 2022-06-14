---
title: Pipeline Runner
parent: Server Deployment
has_children: false
nav_order: 50
---

## {{page.title}}

The MDI apps framework itself carries on app called the 
**Pipeline Runner**. It provides an interactive GUI
you can use to configure and execute job files for
Stage 1 Pipelines, i.e., Pipeline Runner is an 
app that runs pipelines.

### Availability

The Pipeline Runner is only supported on Linux servers,
typically when running the MDI in remote mode, i.e.,
when the app is running on a local computer and the 
web server is running on your HPC server. That configuration
makes it easy to control your HPC server from and app
and to obtain data files for interactive visualization
when your pipeline completes its work.

### Enabling the Pipeline Runner app

Even on a Linux server, you must actively turn on
the Pipeline Runner app as follows:

```yml
# config/stage2-apps.yml
pipeline_runner: true
```


