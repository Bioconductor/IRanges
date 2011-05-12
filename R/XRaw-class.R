### =========================================================================
### XRaw objects
### -------------------------------------------------------------------------
###
### The XRaw class is a container for storing an "external raw vector"
### i.e. a *single* view on a SharedRaw object.
###
### IMPORTANT NOTE: Our concept/representation/implementation of "external
### vector" in general differ significantly from those found in the
### externalVector package!
###

setClass("XRaw",
    contains="XVector",
    representation(
        shared="SharedRaw"
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Initialization.
###

XRaw <- function(length=0L, val=NULL)
{
    if (!isSingleNumber(length) || length < 0L)
        stop("'length' must be a single non-negative integer")
    if (!is.integer(length))
        length <- as.integer(length)
    new2("XRaw", shared=SharedRaw(length=length, val=val), length=length,
         check=FALSE)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

### From standard vectors to XRaw objects:
setAs("raw", "XRaw", function(from) XRaw(length(from), val=from))
setAs("raw", "XVector", function(from) as(from, "XRaw"))
setAs("numeric", "XRaw", function(from) XRaw(length(from), val=from))

### From XRaw objects to standard vectors:
### TODO: Modify SharedRaw.read() so it returns a raw vector instead of a
### character string, and use it here.
setMethod("as.raw", "XRaw", function(x) as.raw(as.integer(x)))
setMethod("as.integer", "XRaw",
    function(x, ...) SharedRaw.readInts(x@shared, x@offset + 1L, x@offset + x@length)
)
setMethod("as.vector", c("XRaw", "missing"),
    function(x, mode) as.raw(x)
)

