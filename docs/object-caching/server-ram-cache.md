---
title: Server RAM Cache
parent: Object Caching
has_children: false
nav_order: 10
---

## {{page.title}}

The MDI apps framework provides a persistent RAM cache,
called **persistentCache**, for 
R data objects that operates at the server level
and is therefore available to all sessions, subject to lifecycle policies.
It is useful for data that is shared between sessions, or 
when a user or developer is likely to reload the app
into the same data set.
It can be used effectively as a session cache, just remembering
that the objects are available to all sessions.

### Fill and access the persistentCache

The `persistentCache` is implemented and used 
through a single function that loads
a pre-existing file into the cache, as follows:

```r
# shiny/shared/global/utilities/cache.R
loadPersistentFile <- function(
    file = NULL,
    sourceId = NULL, 
    contentFileType = NULL, 
    #-----------------------
    force = FALSE,
    ttl = NULL, 
    silent = NULL,
    #-----------------------
    sep = "\t",
    header = TRUE,
    colClasses = NULL,
    #-----------------------
    postProcess = NULL
)
```

where:

- **file** = the path of the file to cache
- **sourceId** = the data package sourceId of the file to cache
- **contentFileType** = the type of the file in `sourceId` to load
- **force** = force the object to be reloaded anew
- **ttl** = time-to-live; how long to cache the object after last access, in seconds
- **silent** = fail silently and return NULL if file not found
- **sep** = passed to fread when loading a csv file
- **header** = passed to fread when loading a csv file
- **colClasses** = passed to fread when loading a csv file; either a character vector or a function that returns one
- **postProcess** = a function applied to data after loading and before caching

The file to cache can be identified by a system file path, or
as the type of the file to load from a specific data package.

If the file is already present in the cache the function
returns quickly. If it is not, the function loads the file
by the appropriate method and returns the same result as described below.

If the file is not found, the function throws an error unless `silent` is TRUE.

All calls to `loadPersistentFile`
clear _all_ files in the cache (not just the one being requested) 
that have exceeded their `ttl` value.

### Accessing data in the loadPersistentFile return value

`loadPersistentFile` returns the system path of the file 
that was just loaded into the cache. That file is the name
of the corresponding element in the server-level list object
`persistentCache` that is available to all apps. The
list element for the file is itself a list with several elements,
the most important of which is the `data` element that has 
the loaded file object.

Thus, a typical usage pattern is:

```r
# <scriptName>.R
filePath <- loadPersistentFile(...)
subset <- persistentCache[[filePath]]$data[1:4, ]
```

where you would  adjust your usage of `persistentCache[[filePath]]$data`
based on the kind of object that was loaded.

Please remember that `loadPersistentFile()` doesn't reload
files and is thus very fast for files already in the cache,
unless `force` is set to TRUE, which is only recommended for debugging.

### Use RDS files for best load times

`loadPersistentFile` will use the `fread` function from data.table
to load flat files such as CSVs. However, it is recommended that
when you first create the file that will be cached that you use a command
like `saveRDS(file)`. The function will then reload it using `readRDS(file)`,
where RDS files are binary files that load noticeably more quickly.

The caching function will save an RDS file for you in anticipation
of speeding future loads, so in the worst case you will suffer
only one slow initial load per file.

### Caution - RAM accumulates and persists

Remember that the purpose of `persistentCache` is that
it is persistent at the server level. This is
rarely a concern in local and remote server modes
where server instances have a finite usage lifetime,
but if a public server runs many apps that put
large amounts of data into the cache, it can accumulate
and crash the server. Strategies to prevent this
include using a session-level cache or being sure
that a session deletes its cached objects `onSessionEnded()`.

More generally, the apps framework enforces a TTL (time-to-live)
policy that deletes cached files that have not been accessed
for a longer time than value `ttl`. Please consider carefully
what an appropriate TTL value is for your app and file.

### Additional references

For complete details, see:

- [mdi-apps-framework : persistentCache](https://github.com/MiDataInt/mdi-apps-framework/blob/main/shiny/shared/global/utilities/cache.R)
