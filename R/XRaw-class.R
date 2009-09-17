### =========================================================================
### XRaw objects
### -------------------------------------------------------------------------
###
### The XRaw class is a container for storing an "external raw vector" i.e.
### a *single* view on a SharedRaw object.
###
### IMPORTANT NOTE: Our concept/representation/implementation of external
### vectors in general differ significantly from those found in the
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

### Ignores the 'drop' argument (so it always behaves like an endomorphism).
setMethod("[", "XRaw",
    function(x, i, j, ..., drop=TRUE)
    {
        if (!missing(j) || length(list(...)) > 0)
            stop("invalid subsetting")
        if (missing(i))
            return(x)
        if (!is.numeric(i) || any(is.na(i)))
            stop("invalid subsetting")
        if (any(i < 1) || any(i > length(x)))
            stop("subscript out of bounds")
        shared <- SharedRaw(length(i))
        SharedRaw.copy(shared, x@offset + i, src=x@shared)
        x@shared <- shared
        x@offset <- 0L
        x@length <- length(shared)
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining.
###
### Should work as an endomorphism (e.g. will return a DNAString instance if
### 'x' is a DNAString instance).
###

setMethod("c", "XRaw",
    function(x, ..., recursive=FALSE)
    {
        if (!identical(recursive, FALSE))
            stop("'recursive' mode not supported")
        if (missing(x)) {
            args <- list(...)
            x <- args[[1]]
        } else {
            args <- list(x, ...)
        }
        if (length(args) == 1L)
            return(x)
        arg_is_null <- sapply(args, is.null)
        if (any(arg_is_null))
            args[arg_is_null] <- NULL  # remove NULL elements by setting them to NULL!
        if (!all(sapply(args, is, class(x))))
            stop("all arguments in '...' must be ", class(x), " objects (or NULLs)")
        ans_length <- sum(sapply(args, length))
        ans_shared <- SharedRaw(ans_length)
        dest_start <- 1L
        for (arg in args) {
            width <- length(arg)
            if (width == 0L)  # will be TRUE on NULLs too...
                next
            ## ... so from here 'arg' is guaranteed to be an XRaw object.
            src <- arg@shared
            src_start <- arg@offset + 1L
            .Call("SharedRaw_memcpy",
                  ans_shared, dest_start, src, src_start, width,
                  PACKAGE="IRanges")
            dest_start <- dest_start + width
        }
        new2(class(x), shared=ans_shared, length=ans_length, check=FALSE)
    }
)

