### =========================================================================
### XDouble objects
### -------------------------------------------------------------------------
###
### The XDouble class is a container for storing an "external double
### vector" i.e. a *single* view on a SharedDouble object.
###
### IMPORTANT NOTE: Our concept/representation/implementation of "external
### vector" in general differ significantly from those found in the
### externalVector package!
###

setClass("XDouble",
    contains="XVector",
    representation(
        shared="SharedDouble"
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Initialization.
###

setMethod("initialize", "XDouble",
    function(.Object, length=0L, val=NULL)
    {
        if (!isSingleNumber(length) || length < 0)
            stop("'length' must be a single non-negative integer")
        if (!is.integer(length))
            length <- as.integer(length)
        .Object@shared <- SharedDouble(length=length, val=val)
        .Object@offset <- 0L
        .Object@length <- length
        .Object
    }
)

XDouble <- function(length=base::length(val), val=NULL)
    new2("XDouble", length=length, val=val, check=FALSE)

### Just an alias for XDouble().
XNumeric <- function(...) XDouble(...)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

### From standard vectors to XDouble objects:
setAs("numeric", "XDouble", function(from) XDouble(length(from), val=from))
setAs("numeric", "XVector", function(from) as(from, "XDouble"))

### From XDouble objects to standard vectors:
setMethod("as.numeric", "XDouble",
    function(x, ...) SharedDouble.read(x@shared, x@offset + 1L, x@offset + x@length)
)
setMethod("as.vector", c("XDouble", "missing"),
    function(x, mode) as.numeric(x)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

### Ignores the 'drop' argument (so it always behaves like an endomorphism).
setMethod("[", "XDouble",
    function(x, i, j, ..., drop=TRUE)
    {
        if (!missing(j) || length(list(...)) > 0)
            stop("invalid subsetting")
        if (missing(i))
            return(x)
        if (!is.numeric(i))
            stop("invalid subsetting")
        if (!is.integer(i))
            i <- as.integer(i)
        shared <- SharedDouble(length(i))
        SharedVector.copy(shared, x@offset + i, src=x@shared)
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
setMethod("==", signature(e1="XDouble", e2="XDouble"),
    function(e1, e2) { e1@shared == e2@shared }
)

