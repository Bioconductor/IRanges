### =========================================================================
### Views objects
### -------------------------------------------------------------------------
###
### The Views class is a general container for representing a set of
### views on an arbitrary object, called the "subject", and for which there
### is a notion of length. For example it can be used to store views on a
### vector or an external vector.
###

setClass("Views",
    contains="UnlockedIRanges",
    representation(
        subject="ANY"
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("subject", function(x) standardGeneric("subject"))

setMethod("subject", "Views", function(x) x@subject)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.Views.width <- function(object)
{
    if (length(object) != 0 && any(width(object) == 0))
        return("null widths are not allowed")
    NULL
}

setValidity("Views",
    function(object)
    {
        problems <- .valid.Views.width(object)
        if (is.null(problems)) TRUE else problems
    }
)

### Need to override the "width" method for IRanges object because of the need
### of an extra check.
setReplaceMethod("width", "Views",
    function(x, check=TRUE, value)
    {
        x <- callNextMethod()
        if (check)
            stopIfProblems(.valid.Views.width(x))
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Initialization.
###

setMethod("initialize", "Views",
    function(.Object, subject=NULL, start=integer(0), end=integer(0), names=NULL)
    {
        if (!isNumericOrNAs(start) || !isNumericOrNAs(end))
            stop("'start' and 'end' must be numerics")
        if (!is.integer(start))
            start <- as.integer(start)
        start[is.na(start)] <- 1L
        if (!is.integer(end))
            end <- as.integer(end)
        end[is.na(end)] <- length(subject)
        if (length(start) < length(end))
            start <- recycleVector(start, length(end))
        else if (length(end) < length(start))
            end <- recycleVector(end, length(start))
        if (!all(start <= end))
            stop("'start' and 'end' must verify 'start <= end'")
        .Object@subject <- subject
        slot(.Object, "start", check=FALSE) <- start
        slot(.Object, "width", check=FALSE) <- end - start + 1L
        names(.Object) <- names
        .Object
    }
)

### User-friendly constructor.
### TODO: Support the 'width' argument.
setGeneric("Views", signature="subject",
    function(subject, start=NA, end=NA, names=NULL) standardGeneric("Views")
)

setMethod("Views", "ANY",
    function(subject, start=NA, end=NA, names=NULL)
        new("Views", subject, start=start, end=end, names=names)
)

views <- function(...) { .Deprecated("Views"); Views(...) }


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setAs("Views", "NormalIRanges",
    function(from) asNormalIRanges(from, check=TRUE)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "restrict" method and the "trim" function.
###

setMethod("restrict", "Views",
    function(x, start, end, keep.all.ranges=FALSE, use.names=TRUE)
    {
        if (!missing(keep.all.ranges))
            stop("argument 'keep.all.ranges' is not supported for Views objects")
        callNextMethod(x, start, end, use.names=use.names)
    }
)

setGeneric("trim", signature="x",
    function(x, use.names=TRUE) standardGeneric("trim")
)

setMethod("trim", "Views",
    function(x, use.names=TRUE)
    {
        y <- restrict(x, start=1L, end=length(subject(x)), use.names=use.names)
        if (length(y) != length(x))
            stop("some views are not overlapping with the subject, cannot trim them")
        y
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "narrow" method and the "subviews" function.
###

setMethod("narrow", "Views",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
    {
        y <- callNextMethod()
        if (any(width(y) == 0))
            stop("some views would have a null width after narrowing")
        y
    }
)

setGeneric("subviews", signature="x",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
        standardGeneric("subviews")
)

setMethod("subviews", "Views",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
        narrow(x, start=start, end=end, width=width, use.names=use.names)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "gaps" method.
###

setMethod("gaps", "Views",
    function(x, start=NA, end=NA)
    {
        if (!isSingleNumberOrNA(start))
            stop("'start' must be a single integer")
        if (!is.integer(start))
            start <- as.integer(start)
        if (!isSingleNumberOrNA(end))
            stop("'end' must be a single integer")
        if (!is.integer(end))
            end <- as.integer(end)
        if (is.na(start))
            start <- 1L
        if (is.na(end))
            end <- length(subject(x))
        callNextMethod(x, start=start, end=end)
    }
)

