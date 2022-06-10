---
title: Server Log
parent: Developer Tools
has_children: false
nav_order: 30
---

## {{page.title}}

One very important code monitoring utility in the global
scope provides feedback in the 
server's command terminal in local and remote modes about the
actions being executed.  

### reportProgress function for production logging

The `reportProgress()` function creates the running
log of server actions you see in local mode. 
It is defined and used as follows:

```r
# logging.R
reportProgress <- function(msg, module = NULL)
```

where:

- **msg** = the message to write to the log stream
- **module** = the name of the calling module or script

For example:

```r
# <scriptName>.R
moduleName <- "myModule"
reportProgress("my log message", moduleName)
```

which would generate the server log message "myModule: my log message".

Such messages are tremendously useful in code development
as they help you understand - in addition to any feedback that R 
gives you - what sequence of actions was taken up to 
the point that code failed. This includes knowing the order
of events and the last reported action.

Keeping such messages in the production app allows you 
to query other users for similar information if they 
are having problems with your app.

### Temporary debug messages

For messages that you do not intend to keep in the production
app, you can use R's standard `message`, `print`,
and `str` commands to provide feedback messages or information
about the objects in use.

There is a tradeoff as it takes a small amount of time for R
to write to the log stream. This is normally negligible 
unless you log a lot of information such as the structure
of complex objects - but the latter can be essential when debugging!
