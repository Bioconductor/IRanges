### =========================================================================
### Views objects
### -------------------------------------------------------------------------
###
### The Views virtual class is a general container for storing a set of views
### on an arbitrary Sequence object, called the "subject".
###

setClass("Views",
    contains="IRanges",
    representation(
        "VIRTUAL",
        subject="Sequence"
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

.valid.Views.width <- function(x)
{
    if (length(width(x)) != 0 && min(width(x)) <= 0)
        return("null widths are not allowed")
    NULL
}

setValidity2("Views", .valid.Views.width)

### Need to override the "width" method for IRanges object because of the extra
### check.
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
### The low-level "Views" constructor.
###
### TODO: - support the 'width' argument;
###       - remove the 'names' argument (user should call names(x) <- somenames
###         separately);
###       - add a 'check.limits' arg (default to TRUE) for raising an error if
###         some views are "out of limits".
###

newViews <- function(subject, start=NA, end=NA, names=NULL, Class=NULL)
{
    if (is(start, "IRanges")) {
        end <- end(start)
        start <- start(start)
    } else if (is(start, "Rle") && length(start) > 0 &&
               is.logical(runValue(start))) {
        whichValues <- which(runValue(start))
        end <- end(start)[whichValues]
        start <- start(start)[whichValues]
    }
    if (!isNumericOrNAs(start) || !isNumericOrNAs(end))
        stop("'start' and 'end' must be numeric vectors")
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
    width <- end - start + 1L
    ## 'start' and 'with' are guaranteed to be valid.
    if (is.null(Class))
        Class <- paste(class(subject), "Views", sep="")
    ans <- new2(Class, subject=subject, start=start, width=width, check=FALSE)
    names(ans) <- names
    ans
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The user-friendly "Views" constructor.
###
### TODO: Same as for the newViews() function above.
###

setGeneric("Views", signature="subject",
    function(subject, start=NA, end=NA, names=NULL) standardGeneric("Views")
)

views <- function(...) { .Deprecated("Views"); Views(...) }


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Extracting a view.
###

### Supported 'i' types: numeric vector of length 1.
setMethod("[[", "Views",
    function(x, i, j, ...)
    {
        if (!missing(j) || length(list(...)) > 0)
            stop("invalid subsetting")
        if (missing(i))
            stop("subscript is missing")
        if (is.character(i))
            stop("cannot subset a ", class(x), " object by names")
        if (!is.numeric(i))
            stop("invalid subscript type")
        if (length(i) < 1L)
            stop("attempt to select less than one element")
        if (length(i) > 1L)
            stop("attempt to select more than one element")
        if (is.na(i))
            stop("subscript cannot be NA")
        if (i == 0)
            return(subject(x))
        if (i < 1L || i > length(x))
            stop("subscript out of bounds")
        start <- start(x)[i]
        end <- end(x)[i]
        if (start < 1L || end > length(subject(x)))
            stop("view is out of limits")
        subseq(subject(x), start=start, end=end)
    }
)

setReplaceMethod("[[", "Views",
    function(x, i, j,..., value)
    {
        stop("attempt to modify the value of a ", class(x), " instance")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

### Unfortunately, even if we've already defined the IRanges->NormalIRanges
### "coerce" method to override the silly implicit one, we still need to
### define the <class>->NormalIRanges ones for every <class> that contains
### IRanges. Otherwise, again, 'as(x, "NormalIRanges")' would call another
### silly implicit method when 'x' is a <class> instance.
### Yes, this is another S4 "feature":
###   https://stat.ethz.ch/pipermail/r-devel/2008-April/049027.html
setAs("Views", "NormalIRanges",
    function(from) asNormalIRanges(from, force=TRUE)
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

### TODO: - add a 'check.limits' arg (default to TRUE) for raising an error if
###         some views are "out of limits"
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "successiveViews" function.
###

successiveViews <- function(subject, width, gapwidth=0, from=1)
{
    views <- successiveIRanges(width, gapwidth=gapwidth, from=from)
    Views(subject, start=start(views), end=end(views))
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "viewApply" function.
###

setGeneric("viewApply", signature="X",
    function(X, FUN, ..., simplify = TRUE) standardGeneric("viewApply")
)

setMethod("viewApply", "Views",
    function(X, FUN, ..., simplify = TRUE)
    {
        X <- trim(X)
        Xsubject <- subject(X)
        Xstart <- start(X)
        Xwidth <- width(X)
        sapply(seq_len(length(X)),
               function(i)
                   FUN(subseq(Xsubject, start = Xstart[i], width = Xwidth[i]),
                       ...),
               simplify = simplify)
    }
)
