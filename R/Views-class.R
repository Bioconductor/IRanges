### =========================================================================
### Views objects
### -------------------------------------------------------------------------
###
### The Views virtual class is a general container for storing a set of views
### on an arbitrary Vector object, called the "subject".
###

setClass("Views",
    contains="IRanges",
    representation(
        "VIRTUAL",
        subject="Vector"
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

newViews <- function(subject, start=NULL, end=NULL, width=NULL, names=NULL,
                     Class=NULL)
{
    if (is(start, "Ranges")) {
        if (!is.null(end) || !is.null(width))
            stop("'end' and 'width' must be NULLs when 'start' is a Ranges object")
        ranges <- start
        ## Keep the names that are already in 'ranges' unless the 'names' arg
        ## was specified.
        if (!is.null(names))
            names(ranges) <- names
    } else {
        ranges <- IRanges(start=start, end=end, width=width, names=names)
    }
    if (is.null(Class))
        Class <- paste(class(subject), "Views", sep="")
    new2(Class, subject=subject,
                start=start(ranges),
                width=width(ranges),
                NAMES=names(ranges),
                check=FALSE)
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
setAs("Vector", "Views",
    function(from) Views(from, start=1L, width=length(from))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Extracting a view.
###

setMethod("[[", "Views",
    function(x, i, j, ...)
    {
        i <- checkAndTranslateDbleBracketSubscript(x, i)
        start <- start(x)[i]
        end <- end(x)[i]
        if (start < 1L || end > length(subject(x)))
            stop("view is out of limits")
        window(subject(x), start=start, end=end)
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
### The "trim" function.
###

setGeneric("trim", signature="x",
    function(x, use.names=TRUE) standardGeneric("trim")
)

setMethod("trim", "Views",
    function(x, use.names=TRUE)
        restrict(x, start=1L, end=length(subject(x)), keep.all.ranges=TRUE,
                 use.names=use.names)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "subviews" function.
###

### TODO: - add a 'check.limits' arg (default to TRUE) for raising an error if
###         some views are "out of limits"
setGeneric("subviews", signature="x",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
        standardGeneric("subviews")
)

setMethod("subviews", "Views",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
        trim(narrow(x, start=start, end=end, width=width, use.names=use.names))
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
    ranges <- successiveIRanges(width, gapwidth=gapwidth, from=from)
    Views(subject, ranges)
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
        ans <-
          sapply(structure(seq_len(length(X)), names = names(X)),
                 function(i)
                     FUN(window(Xsubject, start = Xstart[i], width = Xwidth[i]),
                         ...),
                 simplify = simplify)
        if (!simplify) {
            ans <- newSimpleList("SimpleList", ans, metadata = metadata(X),
                                 elementMetadata = elementMetadata(X))
        }
        ans
    }
)
