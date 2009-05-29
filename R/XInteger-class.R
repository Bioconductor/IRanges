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
        xdata="IntegerPtr"
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
        .Object@xdata <- IntegerPtr(length=length, val=val)
        .Object@offset <- 0L
        .Object@length <- length
        .Object
    }
)

XInteger <- function(length=base::length(val), val=NULL)
    new("XInteger", length=length, val=val)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

### From standard vectors to XInteger objects:
setAs("numeric", "XInteger", function(from) XInteger(length(from), val=from))
setAs("integer", "XSequence", function(from) as(from, "XInteger"))

### From XInteger objects to standard vectors:
setMethod("as.integer", "XInteger",
    function(x, ...) IntegerPtr.read(x@xdata, x@offset + 1L, x@offset + x@length)
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
            return(as.integer(x@xdata))
        }
        if (!is.numeric(i) || any(is.na(i)))
            stop("invalid subsetting")
        if (any(i < 1) || any(i > length(x)))
            stop("subscript out of bounds")
        if (drop)
            return(IntegerPtr.read(x@xdata, x@offset + i))
        xdata <- IntegerPtr(length(i))
        IntegerPtr.copy(xdata, x@offset + i, src=x@xdata)
        x@xdata <- xdata
        x@offset <- 0L
        x@length <- length(xdata)
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Comparison.
###

### FIXME: Compare the contents, not the addresses!
setMethod("==", signature(e1="XInteger", e2="XInteger"),
    function(e1, e2) { e1@xdata == e2@xdata }
)

