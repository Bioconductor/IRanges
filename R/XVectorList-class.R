### =========================================================================
### XVectorList objects
### -------------------------------------------------------------------------
###
### An XVectorList object is *conceptually* a list of XVector objects
### but is actually NOT *implemented* as a list of such objects.
### This is to avoid having to generate long lists of S4 objects which the
### current S4 implementation is *very* inefficient at.
###

setClass("GroupedIRanges",
    contains="IRanges",
    representation(
        group="integer"
    )
)

setClass("XVectorList",
    contains="Sequence",
    representation(
        "VIRTUAL",
        pool="SharedVector_Pool",
        ranges="GroupedIRanges"
    )
)

setClass("XRawList",
    contains="XVectorList",
    representation(
        pool="SharedRaw_Pool"
    ),
    prototype(
        elementType="XRaw"
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### GroupedIRanges methods.
###

.valid.GroupedIRanges <- function(x)
{
    if (length(x@group) != length(x))
        return("slot \"group\" slot must have same length as object")
    NULL
}

setValidity2("GroupedIRanges", .valid.GroupedIRanges)

setMethod("as.data.frame", "GroupedIRanges",
    function(x, row.names=NULL, optional=FALSE, ...)
        cbind(group=x@group, callNextMethod(), stringsAsFactors=FALSE)
)

setMethod("show", "GroupedIRanges",
    function(object) show(as.data.frame(object))
)

setMethod("[", "GroupedIRanges",
    function(x, i, j, ... , drop=TRUE)
    {
        x <- callNextMethod()
        x@group <- x@group[i]
        x
    }
)

setMethod("seqselect", "GroupedIRanges",
    function(x, start=NULL, end=NULL, width=NULL)
    {
        x <- callNextMethod(x, start=start, end=end, width=width)
        x@group <- callGeneric(x@group, start=start, end=end, width=width)
        x
    }
)

setMethod("window", "GroupedIRanges",
    function(x, start = NULL, end = NULL, width = NULL,
             frequency = NULL, delta = NULL, ...)
    {
        x <-
          callNextMethod(x, start=start, end=end, width=width,
                         frequency=frequency, delta=delta, ...)
        x@group <-
          callGeneric(x@group, start=start, end=end, width=width,
                      frequency=frequency, delta=delta, ...)
        x
    }
)

setMethod("c", "GroupedIRanges",
    function(x, ..., recursive=FALSE)
    {
        if (!identical(recursive, FALSE))
            stop("'recursive' argument not supported")
        old_val <- disableValidity()
        on.exit(disableValidity(old_val))
        disableValidity(TRUE)
        ans <- callNextMethod(x, ..., recursive=FALSE)
        ans@group <-
            do.call(c, lapply(list(x, ...), function(arg) arg@group))
        ans
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### XVectorList accessor-like methods.
###

setMethod("length", "XVectorList", function(x) length(x@ranges))

setMethod("width", "XVectorList", function(x) width(x@ranges))

setMethod("names", "XVectorList", function(x) names(x@ranges))

setReplaceMethod("names", "XVectorList",
    function(x, value)
    {
        names(x@ranges) <- value
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### XVectorList constructors.
###

### Takes one XVector object ('xvector') and an IRanges object defining
### 1-based ranges on 'xvector' (conceptually equivalent to defining views
### on subject 'xvector').
unsafe.newXVectorList1 <- function(classname, xvector, ranges)
{
    if (is.null(classname))
        classname <- paste(class(xvector), "List", sep="")
    ans_pool <- as(xvector@shared, "SharedVector_Pool")
    ranges_group <- rep.int(1L, length(ranges))
    ans_ranges <- new2("GroupedIRanges",
                       shift(ranges, xvector@offset),
                       group=ranges_group, check=FALSE)
    new2(classname, pool=ans_pool, ranges=ans_ranges, check=FALSE)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### XVectorList subsetting.
###

XVectorList.getElement <- function(x, i)
{
    ans_class <- elementType(x)
    ans_shared <- x@pool[[x@ranges@group[i]]]
    ans_offset <- x@ranges@start[i] - 1L
    ans_length <- x@ranges@width[i]
    ans <- new2(ans_class,
                shared=ans_shared,
                offset=ans_offset,
                length=ans_length,
                check=FALSE)
    return(ans)
}

setMethod("[[", "XVectorList",
    function(x, i, j, ..., exact=TRUE)
    {
        i <- IRanges:::checkAndTranslateDbleBracketSubscript(x, i)
        XVectorList.getElement(x, i)
    }
)

### TODO: Drop unused pool elements.
setMethod("[", "XVectorList",
    function(x, i, j, ... , drop=TRUE)
    {
        x@ranges <- x@ranges[i]
        x
    }
)

setMethod("seqselect", "XVectorList",
    function(x, start=NULL, end=NULL, width=NULL)
    {
        x@ranges <- callGeneric(x@ranges, start=start, end=end, width=width)
        x
    }
)

setMethod("window", "XVectorList",
    function(x, start = NULL, end = NULL, width = NULL,
             frequency = NULL, delta = NULL, ...)
    {
        x@ranges <-
          callGeneric(x@ranges, start=start, end=end, width=width,
                      frequency=frequency, delta=delta, ...)
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining.
###
### The "c" method for XVectorList objects is implemented to behave like an
### endomorphism i.e. to return an object of the same class as 'x'. In
### particular 'c(x)' returns 'x' and not 'as(x, "XVectorList")'.
### It's easy to implement specific "c" methods for XVectorList subclasses.
### Typically they just need to do something like:
###
###     old_val <- disableValidity()
###     on.exit(disableValidity(old_val))
###     disableValidity(TRUE)
###     ans <- callNextMethod(x, ..., recursive=FALSE)
###     ...
###
### and to take care of the additional slots (aka the subclass-specific
### slots). If there aren't any additional slots (e.g. XRawList), or if the
### additional slots don't need to be modified, then no need to implement a
### specific method at all.
###

### TODO: Merge identical pool elements.
setMethod("c", "XVectorList",
    function(x, ..., recursive=FALSE)
    {
        if (!identical(recursive, FALSE))
            stop("'recursive' argument not supported")
        if (missing(x)) {
            args <- list(...)
            x <- args[[1L]]
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

        for (arg in args[-1L]) {
            ranges <- arg@ranges
            ranges@group <- ranges@group + length(x@pool)
            x@pool <- c(x@pool, arg@pool)
            x@ranges <- c(x@ranges, ranges)
        }
        elementMetadata(x) <- do.call(rbind, lapply(args, elementMetadata))
        validObject(x)
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show method for data column.
###

setMethod("showAsCell", "XVectorList", function(object) as.character(object))
