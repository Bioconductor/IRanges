### =========================================================================
### XInteger objects
### -------------------------------------------------------------------------
###
### The XInteger class is a container for storing an "external integer
### vector" i.e. a *single* view on a SharedInteger object.
###
### IMPORTANT NOTE: Our concept/representation/implementation of "external
### vector" in general differ significantly from those found in the
### externalVector package!
###

setClass("XInteger",
    contains="XVector",
    representation(
        shared="SharedInteger"
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Initialization.
###

setMethod("initialize", "XInteger",
    function(.Object, length=0L, val=NULL)
    {
        if (!isSingleNumber(length) || length < 0)
            stop("'length' must be a single non-negative integer")
        if (!is.integer(length))
            length <- as.integer(length)
        .Object@shared <- SharedInteger(length=length, val=val)
        .Object@offset <- 0L
        .Object@length <- length
        .Object
    }
)

XInteger <- function(length=base::length(val), val=NULL)
    new2("XInteger", length=length, val=val, check=FALSE)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

### From standard vectors to XInteger objects:
setAs("numeric", "XInteger", function(from) XInteger(length(from), val=from))
setAs("integer", "XVector", function(from) as(from, "XInteger"))

### From XInteger objects to standard vectors:
setMethod("as.integer", "XInteger",
    function(x, ...) SharedInteger.read(x@shared, x@offset + 1L, x@offset + x@length)
)
setMethod("as.vector", c("XInteger", "missing"),
    function(x, mode) as.integer(x)
)

