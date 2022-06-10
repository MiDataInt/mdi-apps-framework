---
title: Session Cache Class
parent: Object Caching
has_children: false
nav_order: 20
---

## {{page.title}}

MDI support for object caching at the session
level is implemented as a custom S3 class
called **dataCache**.

### Declaring a new session cache

A new data cache can be declared anywhere in an
app server script, often in _server.R_:

```r
# <scriptName>.R
dataCache <- new_dataCache('cacheName')
```

The returned object has class 'dataCache'. It is a list as follows:

```r
# shiny/shared/global/classes/cache/dataCache/dataCache_class.R
structure(
    list(
        get = get,
        set = set,
        clear = clear,
        clearParentDir = clearParentDir,
        getCacheKeys = getCacheKeys
    ),
    class = 'dataCache'
) 
```

where the critical methods are described below.

### Filling an object into the cache

You add, and also retrieve, an object in the cache
by making a call to the dataCache `get` method:

```r
# shiny/shared/global/classes/cache/dataCache/dataCache_class.R
get <- function(
    type,
    keyObject = NULL, 
    key = NULL,
    permanent = TRUE,
    from = c('ram', 'disk'),
    create = c('asNeeded', 'once', 'always'), 
    createFn = NULL, 
    ... 
)
```

for example:

```r
# <scriptName>.R
dataCache$get('myObject', key = 'abc123', createFn = function(...) ...) 
```

where:

- **type** = a human readable name for the kind of object this is within its parentType
- **keyObject** = any R object that can be hashed to a key that defines the data to be cached
- **key** = a string key to be used instead of `keyObject`
- **permanent** =  make a disk as well as a RAM copy; permanent==FALSE is incompatible with from=='disk'
- **from** =  determines where the cache is allowed to get data; disk disables the RAM cache
- **create** = determines when we are obliged to call createFn
- **createFn** = for missing/potentially stale objects, call `createFn` to create them anew
- **...** =  optional named arguments passed to `createFn`, along with cacheKey, keyObject, key, cacheObject

Either a key object or a key must be provided. `key` is a string value that is the 
cache key for the data. Otherwise, `keyObject` will be hashed to create a unique
key. Either way, the resulting key must have a one-to-one 
mapping to the data to be cached.

The combination of `permanent` and `from` determine how a RAM vs. a disk
cache is used. The most typical and default usage stores cached objects on the disk
for future access but places them in RAM on first load for rapid recovery on
subsequent calls in the same session.

The object to be cached must be returned by `createFn`, which can act however
is needed by your app. The value of `create` determines when the cache code
will call `createFn`. By default, it is only called once per cache key, preferring
to use cached objects, even if they must be recovered from disk.

### Accessing the value of a cached object

`dataCache$get` returns an object known as a cacheObject, which is a
list, where:

- **value** = the data payload of the object
- **cacheKey** = the object's cache key
- **timestamp** = when the object was cached
- **keyObject** = the same as in the get call, repeated back for subsequent examination

for example:

```r
# <scriptName>.R
cacheObject <- dataCache$get('myObject', key = 'abc123', createFn = function(...) ...) 
plot(cacheObject$value)
```

### Replacing a cached data value

You can force a new value for a cached object using the dataCache `set` method:

```r
# shiny/shared/global/classes/cache/dataCache/dataCache_class.R
set <- function(
    cacheObject, 
    newValue = NULL
)
```

where:

- **cacheObject** = a cacheObject previously returned by `get`
- **newValue** = the new value to assign

for example:

```r
# <scriptName>.R
cacheObject <- dataCache$get('myObject', key = 'abc123', createFn = function(...) ...) 
dataCache$set(cacheObject, 'xyz')
```

### Clearing a cached object

You can clear one or more cache keys from the dataCache using
the dataCache  `clear` method:

```r
# shiny/shared/global/classes/cache/dataCache/dataCache_class.R
clear <- function(
    cacheKeys = NULL, # defaults to all objects
    purgeFiles = FALSE
)
```

where:

- **cacheKeys** = a vector of one or more cacheKey values
- **purgeFiles** = whether to remove the disk cache files also

for example:

```r
# <scriptName>.R
cacheObject <- dataCache$get('myObject', key = 'abc123', createFn = function(...) ...) 
dataCache$clear(cacheObject$cacheKey, purgeFiles = TRUE)
```

The default behavior clears all cached objects from RAM only.

### Additional references

For complete details, see:

- [mdi-apps-framework : dataCache_class](https://github.com/MiDataInt/mdi-apps-framework/blob/main/shiny/shared/global/classes/cache/dataCache/dataCache_class.R)
