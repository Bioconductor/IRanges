### =========================================================================
### XInteger objects
### -------------------------------------------------------------------------
###
### The XInteger class is a container for storing an external sequence
### of integers (stored as int values at the C level).
###

setClass("XInteger",
    contains="XSequence",
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
setAs("integer", "XSequence", function(from) as(from, "XInteger"))

### From XInteger objects to standard vectors:
setMethod("as.integer", "XInteger",
    function(x, ...) SharedInteger.read(x@shared, x@offset + 1L, x@offset + x@length)
)
setMethod("as.vector", c("XInteger", "missing"),
    function(x, mode) as.integer(x)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

setMethod("[", "XInteger",
    function(x, i, j, ..., drop=TRUE)
    {
        if (!missing(j) || length(list(...)) > 0)
            stop("invalid subsetting")
        if (missing(i)) {
            if (!drop)
                return(x)
            return(as.integer(x@shared))
        }
        if (!is.numeric(i) || any(is.na(i)))
            stop("invalid subsetting")
        if (any(i < 1) || any(i > length(x)))
            stop("subscript out of bounds")
        if (drop)
            return(SharedInteger.read(x@shared, x@offset + i))
        shared <- SharedInteger(length(i))
        SharedInteger.copy(shared, x@offset + i, src=x@shared)
        x@shared <- shared
        x@offset <- 0L
        x@length <- length(shared)
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Comparison.
###

### FIXME: Compare the contents, not the addresses!
setMethod("==", signature(e1="XInteger", e2="XInteger"),
    function(e1, e2) { e1@shared == e2@shared }
)

