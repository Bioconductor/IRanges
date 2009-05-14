### =========================================================================
### Views objects
### -------------------------------------------------------------------------
###
### The Views virtual class is a general container for storing a set of views
### on an arbitrary Sequence object, called the "subject".
###

setClass("Views",
    contains=c("IRanges", "ListLike"),
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
    if (length(width(x)) != 0L && min(width(x)) <= 0L)
        return("null widths are not allowed")
    NULL
}

setValidity2("Views", .valid.Views.width)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The low-level "Views" constructor.
###
### TODO: - add a 'check.limits' arg (default to TRUE) for raising an error if
###         some views are "out of limits".
###

newViews <- function(subject, start=NULL, end=NULL, width=NULL, names=NULL, Class=NULL)
{
    if (is(start, "IRanges")) {
        if (!is.null(end) || !is.null(width))
            stop("'end' and 'width' must be NULLs when 'start' is an IRanges object")
        ir <- start
        ## Keep names in 'ir' unless 'names' is specified.
        if (!is.null(names))
            names(ir) <- names
    } else {
        ir <- IRanges(start=start, end=end, width=width, names=names)
    }
    if (length(width(ir)) != 0L && min(width(ir)) <= 0L)
        stop("null widths are not allowed")
    if (is.null(Class))
        Class <- paste(class(subject), "Views", sep="")
    #new2(Class, ir, subject=subject, check=FALSE)  # gives me an infinite recursion!
    new2(Class, subject=subject, start=start(ir), width=width(ir), NAMES=names(ir), check=FALSE)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The user-friendly "Views" constructor.
###
### TODO: Same as for the newViews() function above.
###

setGeneric("Views", signature="subject",
    function(subject, start=NULL, end=NULL, width=NULL, names=NULL)
        standardGeneric("Views")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

### Returns a single view covering the entire sequence.
setAs("Sequence", "Views",
    function(from) Views(from, start=1L, width=length(from))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Extracting a view.
###

setMethod("[[", "Views",
    function(x, i, j, ...)
    {
        i <- callNextMethod()
        start <- start(x)[i]
        end <- end(x)[i]
        if (start < 1L || end > length(subject(x)))
            stop("view is out of limits")
        window(subject(x), start=start, end=end)
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
                   FUN(window(Xsubject, start = Xstart[i], width = Xwidth[i]),
                       ...),
               simplify = simplify)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Old stuff (Deprecated or Defunct).
###

views <- function(...) .Defunct("Views")

