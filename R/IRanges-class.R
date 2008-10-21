### =========================================================================
### IRanges objects
### -------------------------------------------------------------------------
###
### The IRanges class is a simple container for storing a set of integer
### ranges.
###

setClass("IRanges",
    contains="Ranges",
    representation(
        start="integer",
        width="integer",
        NAMES="characterORNULL",  # R doesn't like @names !!
        is_locked="logical" # no code uses this slot for now!
    ),
    prototype(
        start=integer(0),
        width=integer(0),
        NAMES=NULL,
        is_locked=FALSE
    )
)

### A NormalIRanges object is an IRanges object where the ranges are:
###   (a) not empty (i.e. they have a non-null width);
###   (b) not overlapping;
###   (c) ordered from left to right;
###   (d) not even adjacent (i.e. there must be a non empty gap between 2
###       consecutive ranges).
### If 'x' is an IRanges object of length >= 2, then 'x' is normal iff:
###   start(x)[i] <= end(x)[i] < start(x)[i+1] <= end(x)[i+1]
### for every 1 <= i < length(x).
### If length(x) == 1, then 'x' is normal iff width(x)[1] >= 1.
### If length(x) == 0, then 'x' is normal.
setClass("NormalIRanges", contains="IRanges")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("length", "IRanges", function(x) length(start(x)))

setMethod("start", "IRanges", function(x, ...) x@start)

setMethod("width", "IRanges", function(x) x@width)

setMethod("names", "IRanges", function(x) x@NAMES)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "isNormal" and "whichFirstNotNormal" generics and methods.
###
### NOTE: Should they be part of the Ranges API?
###

setGeneric("isNormal", function(x) standardGeneric("isNormal"))

setMethod("isNormal", "IRanges",
    function(x)
    {
        all_ok <- all(width(x) >= 1)
        if (length(x) >= 2)
            all_ok <- all_ok && all(start(x)[-1] - end(x)[-length(x)] >= 2)
        all_ok
    }
)

setGeneric("whichFirstNotNormal", function(x) standardGeneric("whichFirstNotNormal"))

setMethod("whichFirstNotNormal", "IRanges",
    function(x)
    {
        is_ok <- width(x) >= 1
        if (length(x) >= 2)
            is_ok <- is_ok & c(TRUE, start(x)[-1] - end(x)[-length(x)] >= 2)
        which(!is_ok)[1]
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "isEmpty" methods.
###
### An IRanges object is considered empty iff all its ranges are empty.
### 

setMethod("isEmpty", "Ranges", function(x) all(width(x) == 0))

### The "isEmpty" method for IRanges objects would work fine on
### NormalIRanges objects but it can be made faster.
setMethod("isEmpty", "NormalIRanges", function(x) length(x) == 0)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "max" and "min" methods.
###
### Note: defined for NormalIRanges objects only.
### For an ordinary IRanges object 'x', it's not clear what the semantic
### should be. In particular, should empty ranges be ignored or not? If not
### then we could end up with 'min(x)' > 'max(x)' (e.g. when 'x' is made of 1
### empty range) which is not nice. Another (and more pragmatic) reason for
### not defining these methods for IRanges objects is that I don't need them
### at the moment.
###

setMethod("max", "NormalIRanges",
    function(x, ..., na.rm)
    {
        if (isEmpty(x)) {
            warning("empty ", class(x), " object; returning -Inf")
            -Inf
        } else {
            end(x)[length(x)]
        }
    }
)

setMethod("min", "NormalIRanges",
    function(x, ..., na.rm)
    {
        if (isEmpty(x)) {
            warning("empty ", class(x), " object; returning Inf")
            Inf
        } else {
            start(x)[1]
        }
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###
### Both 'start' and 'width' must be integer vectors of equal length
### (eventually 0) with no NAs and such that all(width >= 0) is TRUE.
### 'names' must be NULL or a character vector of the same length as 'start'
### (or 'width').
###
### We use 'min(width(x)) < 0L' in .valid.IRanges.width().
### Note that the 'min(x) <= y' construct is faster and more memory efficent
### than 'any(x <= y)', especially when 'x' is a big vector (the speedup is
### around 10x or more when length(x) >= 100000).
###

### IRanges objects

.valid.IRanges.start <- function(x)
{
    if (!is.integer(start(x)) || any(is.na(start(x))))
        return("the starts must be non-NA integers")
    if (length(start(x)) != length(width(x)))
        return("number of starts and number of widths differ")
    NULL
}

.valid.IRanges.width <- function(x)
{
    if (!is.integer(width(x)) || any(is.na(width(x))))
        return("the widths must be non-NA integers")
    if (length(start(x)) != length(width(x)))
        return("number of starts and number of widths differ")
    if (length(width(x)) != 0 && min(width(x)) < 0)
        return("negative widths are not allowed")
    NULL
}

.valid.IRanges.names <- function(x)
{
    if (is.null(names(x)))
        return(NULL)
    if (!is.character(x@NAMES))
        return("the 'NAMES' slot must contain a character vector")
    if (length(names(x)) != length(x))
        return("number of names and number of elements differ")
    NULL
}

.valid.IRanges <- function(x)
{
    c(.valid.IRanges.start(x),
      .valid.IRanges.width(x),
      .valid.IRanges.names(x))
}

setValidity2("IRanges", .valid.IRanges)

### NormalIRanges objects

.valid.NormalIRanges <- function(x)
{
    if (!isNormal(x))
        return("object is not normal")
    NULL
}

setValidity2("NormalIRanges", .valid.NormalIRanges)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The safe and user-friendly "IRanges" constructor.
###

### Return NULL or an integer vector with no NAs.
.IRanges.normargStartEndWidth <- function(start_end_width, argname)
{
    if (is.null(start_end_width))
        return(start_end_width)
    if (!is.numeric(start_end_width))
        stop("'", argname, "' must be a numeric vector (or NULL)")
#    if (length(start_end_width) == 0)
#        stop("'", argname, "' must contain at least one integer value")
    if (!is.integer(start_end_width))
        start_end_width <- as.integer(start_end_width)
    if (any(is.na(start_end_width)))
        stop("'", argname,, "' cannot contain NAs")
    start_end_width
}

IRanges <- function(start=NULL, end=NULL, width=NULL)
{
    start <- .IRanges.normargStartEndWidth(start, "start")
    end <- .IRanges.normargStartEndWidth(end, "end")
    width <- .IRanges.normargStartEndWidth(width, "width")
    null_args <- c(is.null(start), is.null(end), is.null(width))
    if (all(null_args))
        return(new("IRanges"))
    if (sum(!null_args) != 2)
        stop("exactly two out of the 'start', 'end' and 'width' arguments must be specified")
    if (is.null(width)) { 
        if (length(start) != length(end))
            stop("'start' and 'end' must have the same length")
        if (length(start) != 0)
            width <- end - start + 1L
        else
            width <- integer(0)
    } else if (is.null(start)) {
        if (length(width) > length(end))
            stop("'width' has more elements than 'end'")
        if (length(width) != 0)
            start <- end - width + 1L
        else if (length(end) == 0)
            start <- integer(0)
        else
            stop("cannot recycle zero length 'width'")
    } else {
        if (length(width) > length(start))
            stop("'width' has more elements than 'start'")
        if (length(width) < length(start)) {
            if (length(width) != 0)
                width <- rep(width, length.out=length(start))
            else
                stop("cannot recycle zero length 'width'")
        }
    }
    ## 'start' and 'with' are guaranteed to be valid.
    new2("IRanges", start=start, width=width, check=FALSE)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

### Helper function (not exported) used by the "coerce" methods defined in
### IRanges-utils.R. Believe it or not but the implicit "coerce" methods do
### NOT check that they return a valid object!
newNormalIRangesFromIRanges <- function(x, check=TRUE)
{
    if (!is(x, "IRanges"))
        stop("'x' must be an IRanges object")
    ## Check only what needs to be checked.
    if (check)
        stopIfProblems(.valid.NormalIRanges(x))
    ## Make a "hard copy" of the slots. No need to check anything!
    new2("NormalIRanges", start=x@start, width=x@width, NAMES=x@NAMES, check=FALSE)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Low-level (i.e. non-exported and unsafe) replacement functions for
### IRanges objects.
###
### The choice was made to implement a "resizing" semantic:
###   (1) changing the start preserves the end (so it changes the width)
###   (2) changing the end preserves the start (so it changes the width)
###   (3) changing the width preserves the start (so it changes the end)
###
### IMPORTANT: They do NOT check their arguments ('x' and 'value'). In
### particular they do not check that 'value' is of the expected type (integer
### for "unsafe.start<-", "unsafe.width<-", "unsafe.end<-", and character for
### "unsafe.names<-"). Also they don't check that the resulting IRanges object
### is valid!
###

### 'value' is recycled.
`unsafe.start<-` <- function(x, value)
{
    old_start <- start(x)
    ## Use 'x@start[]' instead of just 'x@start' so the right value is recycled
    x@start[] <- numeric2integer(value)
    x@width <- width(x) - start(x) + old_start
    x
}

### 'value' is recycled.
`unsafe.end<-` <- function(x, value)
{
    ## Use 'x@width[]' instead of just 'x@width' so the right value is recycled
    x@width[] <- width(x) + numeric2integer(value) - end(x)
    x
}

### 'value' is recycled.
`unsafe.width<-` <- function(x, value)
{
    ## Use 'x@width[]' instead of just 'x@width' so the right value is recycled
    x@width[] <- numeric2integer(value)
    x
}

### 'value' is NOT recycled so we stay close to what the standard R "names<-"
### methods generally do
`unsafe.names<-` <- function(x, value)
{
    if (is.null(value))
        x@NAMES <- NULL
    else {
        if (length(value) > length(x))
            stop("too many names")
        if (length(value) < length(x))
            value <- c(value, rep(NA, length(x) - length(value)))
        x@NAMES <- value
    }
    x
}

unsafe.update <- function(object, ...)
{
    valid_argnames <- c("start", "end", "width", "names")
    args <- extraArgsAsList(valid_argnames, ...)
    argnames <- names(args)
    sew <- c("start", "end", "width")
    narg_in_sew <- sum(sew %in% argnames)
    if (narg_in_sew == 3)
        stop("only two of the ",
             paste("'", sew, "'", sep="", collapse=", "),
             " arguments can be specified")
    do_atomic_update <- narg_in_sew == 2 && (is.null(names(object))
                                             || ("names" %in% argnames))
    if (do_atomic_update) {
        if ("end" %in% argnames) {
            if ("width" %in% argnames) {
                width <- args$width
                start <- args$end - width + 1L
            } else {
                start <- args$start
                width <- args$end - start + 1L
            }
        } else {
            start <- args$start
            width <- args$width
        }
        object@start <- numeric2integer(start)
        object@width <- numeric2integer(width)
        object@NAMES <- args$names
        return(object)
    }
    if ("start" %in% argnames)
        unsafe.start(object) <- args$start
    if ("end" %in% argnames)
        unsafe.end(object) <- args$end
    if ("width" %in% argnames)
        unsafe.width(object) <- args$width
    if ("names" %in% argnames)
        unsafe.names(object) <- args$names
    object
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Exported (and safe) replacement methods.
###
### See the unsafe replacement functions for IRanges objects above for the
### "sliding rules".
###
### Note that we don't call validObject(x) after 'x' has been modified because
### we don't need to revalidate the entire object: validating the bits that
### have been touched is enough (and faster). However, because of this, when
### defining a new class that contains the IRanges class, if objects of
### the new class must satisfy additional constraints, then some of the
### replacement methods below need to be overridden for this new class.
###

setReplaceMethod("start", "IRanges",
    function(x, check=TRUE, value)
    {
        unsafe.start(x) <- value
        if (check)
            stopIfProblems(c(.valid.IRanges.start(x), .valid.IRanges.width(x)))
        x
    }
)

setReplaceMethod("width", "IRanges",
    function(x, check=TRUE, value)
    {
        unsafe.width(x) <- value
        if (check)
            stopIfProblems(.valid.IRanges.width(x))
        x
    }
)

setReplaceMethod("end", "IRanges",
    function(x, check=TRUE, value)
    {
        unsafe.end(x) <- value
        if (check)
            stopIfProblems(.valid.IRanges.width(x))
        x
    }
)

### Yes, for IRanges objects!
setReplaceMethod("names", "IRanges",
    function(x, value)
    {
        if (!is.character(value) && !is.null(value)) {
            stop("'value' must be NULL or a character vector")
        }
        unsafe.names(x) <- value
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "update" method.
###
### This is a convenience method for combining multiple modifications in one
### single call.
###
### It must verify 2 important properties:
###   (1) update(x) must be identical to x (doesn't touch x at all)
###   (2) update(x, start=start(x), width=width(x), names=names(x))
###       must be identical to x too (but this time it updates x with its own
###       content)
###

setMethod("update", "IRanges",
    function(object, ...)
    {
        object <- unsafe.update(object, ...)
        check <- list(...)$check
        if (is.null(check))
            check <- TRUE
        if (check)
            validObject(object)
        object
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

### Supported 'i' types: numeric vector, logical vector, NULL and missing.
setMethod("[", "IRanges",
    function(x, i, j, ..., drop)
    {
        if (!missing(j) || length(list(...)) > 0)
            stop("invalid subsetting")
        if (missing(i))
            return(x)
        if (!is.atomic(i))
            stop("invalid subscript type")
        if (is.character(i))
            stop("cannot subset a ", class(x), " object by names")
        lx <- length(x)
        if (is.numeric(i)) {
            if (any(is.na(i)))
                stop("subscript contains NAs")
            if (any(i < -lx) || any(i > lx))
                stop("subscript out of bounds")
            if (is(x, "NormalIRanges") && all(i >= 0)) {
                if (!is.integer(i))
                    i <- as.integer(i)
                i <- i[i != 0]
                if (isNotStrictlySorted(i))
                    stop("positive numeric subscript must be strictly increasing ",
                         "for NormalIRanges objects")
            }
        } else if (is.logical(i)) {
            if (any(is.na(i)))
                stop("subscript contains NAs")
            if (length(i) > lx)
                stop("subscript out of bounds")
        } else if (!is.null(i)) {
            stop("invalid subscript type")
        }
        slot(x, "start", check=FALSE) <- start(x)[i]
        slot(x, "width", check=FALSE) <- width(x)[i]
        if (!is.null(names(x)))
            slot(x, "NAMES", check=FALSE) <- names(x)[i]
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Splitting and combining.
###

setMethod("split", "IRanges",
    function(x, f, drop = FALSE, ...)
    {
        do.call("RangesList", callNextMethod())
    }
)

setMethod("c", "IRanges",
    function(x, ..., recursive = FALSE)
    {
        if (recursive)
            stop("'recursive' mode not supported")
        if (!all(sapply(list(...), is, "IRanges")))
            stop("all arguments in '...' must be instances of IRanges")
        if (!missing(x))
            args <- list(x, ...)
        else
            args <- list(...)
        IRanges(unlist(lapply(args, start), use.names=FALSE),
                unlist(lapply(args, end), use.names=FALSE))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Old stuff (Defunct or Deprecated).
###

setGeneric("first", function(x) standardGeneric("first"))
setMethod("first", "IRanges", function(x) {.Defunct("start"); start(x)})
setGeneric("last", function(x) standardGeneric("last"))
setMethod("last", "IRanges", function(x) {.Defunct("end"); end(x)})

