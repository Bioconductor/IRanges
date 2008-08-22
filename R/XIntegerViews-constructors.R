### =========================================================================
### Constructor-like functions and generics for IntegerViews objects
### -------------------------------------------------------------------------

### The "views" function has the following properties:
###   - It is exported (and safe).
###   - First argument is 'subject'. It must be an integer vector or an
###     XInteger object.
###   - Passing something else to 'subject' provokes an error.
###   - They return an XIntegerViews object whose 'subject' slot is the object
###     passed in the 'subject' argument.

setMethod("views", signature = c(subject = "integer"),
    function(subject, start=NA, end=NA)
    {
        x <- XInteger(length(subject))
        x[] <- subject
        views(x, start = start, end = end)
	}
)
setMethod("views", signature = c(subject = "XInteger"),
    function(subject, start=NA, end=NA)
    {
        ans <- new("XIntegerViews", subject, check=FALSE)
        ranges <- .safeMakeViews(subject(ans), start, end)
        update(ans, start=start(ranges), width=width(ranges))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "XIntegerViews" generic and methods.

setGeneric("XIntegerViews", signature="x",
    function(x, start=NA, end=NA) standardGeneric("XIntegerViews")
)
setMethod("XIntegerViews", "integer", function(x, start=NA, end=NA) views(x, start=NA, end=NA))
setMethod("XIntegerViews", "XInteger", function(x, start=NA, end=NA) views(x, start=NA, end=NA))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "restrict" method and the "trim" function.
###

setMethod("restrict", "XIntegerViews",
    function(x, start, end, keep.all.ranges=FALSE, use.names=TRUE)
    {
        if (!missing(keep.all.ranges))
            stop("'keep.all.ranges' is not supported for XIntegerViews objects")
        callNextMethod(x, start, end, use.names=use.names)
    }
)

setMethod("trim", "XIntegerViews",
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

setMethod("narrow", "XIntegerViews",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
    {
        y <- callNextMethod()
        if (any(width(y) == 0))
            stop("some views would have a null width after narrowing")
        y
    }
)

setMethod("subviews", "XIntegerViews",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
    {
        narrow(x, start=start, end=end, width=width, use.names=use.names)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "gaps" method.
###

setMethod("gaps", "XIntegerViews",
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "slice" generic and method.
###

setGeneric("slice", signature="x",
    function(x, lower=-Inf, upper=Inf, includeLower=TRUE, includeUpper=TRUE)
        standardGeneric("slice")
)

setMethod("slice", "integer",
    function(x, lower=-.Machine$integer.max, upper=.Machine$integer.max, includeLower=TRUE, includeUpper=TRUE)
        slice(XInteger(x), lower=lower, upper=upper, includeLower=includeLower,
              includeUpper=includeUpper)
)

setMethod("slice", "XInteger",
    function(x, lower=-.Machine$integer.max, upper=.Machine$integer.max, includeLower=TRUE, includeUpper=TRUE)
    {
        if (!isSingleNumber(lower))
            stop("'lower' must be a single integer")
        if (!is.integer(lower))
            lower <- as.integer(lower)
        if (!isSingleNumber(upper))
            stop("'upper' must be a single integer")
        if (!is.integer(upper))
            upper <- as.integer(upper)
        .Call("XIntegerViews_slice", x, lower, upper, includeLower, includeUpper, PACKAGE="IRanges")
    }
)
