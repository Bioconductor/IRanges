### =========================================================================
### XRaw objects
### -------------------------------------------------------------------------
###
### The XRaw class is a container for storing an external sequence of
### bytes (stored as char values at the C level).
###

setClass("XRaw",
    contains="XSequence",
    representation(
        xdata="RawPtr"
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
    new("XRaw", xdata=RawPtr(length=length, val=val), length=length)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setMethod("as.integer", "XRaw",
    function(x) RawPtr.readInts(x@xdata, x@offset + 1L, x@offset + x@length)
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
        xdata <- RawPtr(length(i))
        RawPtr.copy(xdata, x@offset + i, src=x@xdata)
        x@xdata <- xdata
        x@offset <- 0L
        x@length <- length(xdata)
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
        if (!all(sapply(list(...),
                        function(arg) {is.null(arg) || is(arg, class(x))})))
            stop("all arguments in '...' must be ", class(x), " objects or NULLs")
        args <- list(x, ...)
        ans_length <- sum(sapply(args, length))
        ans_xdata <- RawPtr(ans_length)
        dest_start <- 1L
        for (arg in args) {
            width <- length(arg)
            if (width == 0L)  # will be TRUE on NULLs too...
                next
            ## ... so from here 'arg' is guaranteed to be an XRaw object.
            src <- arg@xdata
            src_start <- arg@offset + 1L
            .Call("RawPtr_memcpy",
                  ans_xdata, dest_start, src, src_start, width,
                  PACKAGE="IRanges")
            dest_start <- dest_start + width
        }
        new(class(x), xdata=ans_xdata, length=ans_length)
    }
)

