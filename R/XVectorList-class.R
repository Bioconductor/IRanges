### =========================================================================
### XVectorList objects
### -------------------------------------------------------------------------
###
### An XVectorList object is *conceptually* a list of XVector objects
### but is actually not *implemented* as a list of such objects.
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
    contains="List",
    representation(
        "VIRTUAL",
        pool="SharedVector_Pool",
        ranges="GroupedIRanges"
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
        if (!is.null(end) || !is.null(width))
            start <- IRanges(start = start, end = end, width = width)
        irInfo <- .bracket.Index(start, length(x), names(x), asRanges = TRUE)
        if (!is.null(irInfo[["msg"]]))
            stop(irInfo[["msg"]])
        if (irInfo[["useIdx"]]) {
            ir <- irInfo[["idx"]]
            slot(x, "group", check=FALSE) <- callGeneric(x@group, ir)
            x <- callNextMethod(x, ir)
        }
        x
    }
)

setMethod("window", "GroupedIRanges",
    function(x, start=NA, end=NA, width=NA,
             frequency=NULL, delta=NULL, ...)
    {
        x <- callNextMethod(x, start=start, end=end, width=width,
                            frequency=frequency, delta=delta)
        x@group <- window(x@group, start=start, end=end, width=width,
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
            do.call(c, lapply(unname(list(x, ...)), function(arg) arg@group))
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

### Produces an XVectorList object of the given length with empty elements.
XVectorList <- function(classname, length=0L)
{
    elt <- new(elementType(new(classname)))
    ans1_pool <- as(elt@shared, "SharedVector_Pool")
    ans1_ranges <- new("GroupedIRanges", IRanges(start=1L, width=0L), group=1L)
    ans1 <- new2(classname, pool=ans1_pool, ranges=ans1_ranges, check=FALSE)
    rep.int(ans1, length)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### 2 internal bookkeeping functions to keep the XVectorList "pool" slot
### clean and tidy.
###

### Used in "[" method for XVectorList objects.
.dropUnusedPoolElts <- function(x)
{
    pool_len <- length(x@pool)
    if (pool_len == 0L)
        return(x)
    keep_it <- logical(pool_len)
    keep_it[x@ranges@group] <- TRUE
    keep_idx <- which(keep_it)
    remap <- integer(pool_len)
    remap[keep_idx] <- seq_len(length(keep_idx))
    x@pool <- x@pool[keep_idx]
    x@ranges@group <- remap[x@ranges@group]
    x
}

### Used in "c" method for XVectorList objects.
.dropDuplicatedPoolElts <- function(x)
{
    pool_len <- length(x@pool)
    if (pool_len == 0L)
        return(x)
    remap <- high2low(sapply(x@pool@xp_list, address))
    keep_idx <- which(is.na(remap))
    remap[keep_idx] <- seq_len(length(keep_idx))
    x@pool <- x@pool[keep_idx]
    x@ranges@group <- remap[x@ranges@group]
    x
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
        i <- checkAndTranslateDbleBracketSubscript(x, i)
        XVectorList.getElement(x, i)
    }
)

### Always behaves like an endomorphism (i.e. ignores the 'drop' argument and
### behaves like if it was actually set to FALSE).
setMethod("[", "XVectorList",
    function(x, i, j, ... , drop=TRUE)
    {
        if (!missing(j) || length(list(...)) > 0L)
            stop("invalid subsetting")
        if (missing(i)) 
            i <- seq_len(length(x))
        else
            i <- normalizeSingleBracketSubscript(i, x)
        x@ranges <- x@ranges[i]
        mcols(x) <- mcols(x)[i, , drop=FALSE]
        ## Drop unused pool elements.
        x <- .dropUnusedPoolElts(x)
        x
    }
)

setMethod("seqselect", "XVectorList",
    function(x, start=NULL, end=NULL, width=NULL)
    {
        x@ranges <- seqselect(x@ranges, start=start, end=end, width=width)
        mcols(x) <- seqselect(mcols(x),
                                        start=start, end=end, width=width)
        x
    }
)

setMethod("window", "XVectorList",
    function(x, start=NA, end=NA, width=NA, frequency=NULL, delta=NULL, ...)
    {
        x@ranges <- window(x@ranges, start=start, end=end, width=width,
                           frequency=frequency, delta=delta, ...)
        mcols(x) <- window(mcols(x),
                                     start=start, end=end, width=width,
                                     frequency=frequency, delta=delta, ...)
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### subseq()
###

narrowXVectorList <- function(x, start=NA, end=NA, width=NA, use.names=TRUE)
{
    x@ranges <- narrow(x@ranges, start=start, end=end, width=width,
                       use.names=use.names)
    x
}

setMethod("subseq", "XVectorList",
    function(x, start=NA, end=NA, width=NA)
        narrowXVectorList(x, start=start, end=end, width=width)
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

setMethod("c", "XVectorList",
    function(x, ..., recursive=FALSE)
    {
        if (!identical(recursive, FALSE))
            stop("'recursive' argument not supported")
        if (missing(x)) {
            args <- unname(list(...))
            x <- args[[1L]]
        } else {
            args <- unname(list(x, ...))
        }
        if (length(args) == 1L)
            return(x)
        arg_is_null <- sapply(args, is.null)
        if (any(arg_is_null))
            args[arg_is_null] <- NULL  # remove NULL elements by setting them to NULL!
        if (!all(sapply(args, is, class(x))))
            stop("all arguments in '...' must be ", class(x), " objects (or NULLs)")
        ## Combine the "pool" and "ranges" slots.
        for (arg in args[-1L]) {
            ranges <- arg@ranges
            ranges@group <- ranges@group + length(x@pool)
            x@pool <- c(x@pool, arg@pool)
            x@ranges <- c(x@ranges, ranges)
        }
        ## Combine the metadata columns.
        mcols(x) <- do.call(rbind, lapply(args, mcols))
        ## Drop duplicated pool elements.
        x <- .dropDuplicatedPoolElts(x)
        validObject(x)
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Replacement methods.
###

setReplaceMethod("[", "XVectorList",
    function(x, i, j,..., value)
    {
        ans <- c(x, value)
        idx <- seq_len(length(x))
        idx[i] <- length(x) + seq_len(length(value))
        ans <- ans[idx]
        names(ans) <- names(x)
        ans
    }
)

setReplaceMethod("[[", "XVectorList",
    function(x, i, j, ..., value)
    {
        i <- checkAndTranslateDbleBracketSubscript(x, i)
        if (!is(value, elementType(x)))
            stop("supplied replacement value must be a ",
                 elementType(x), " object")
        x[i] <- as(value, class(x))
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show method for data column.
###

setMethod("showAsCell", "XVectorList", function(object) as.character(object))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Two helper functions for unlisting and unsplitting a list of XVectorList
### objects. Not for the end user.
###

unlist_list_of_XVectorList <- function(classname, x)
{
    ## Prepending the list with 'new(classname)' guarantees that we dispatch
    ## on the right "c" method, even when 'x' is an empty list. Note that this
    ## code would work on a list of objects with another type, not only
    ## XVectorList objects, as long as they have a "c" method.
    do.call(c, c(list(new(classname)), unname(x)))
}

### 'f' must be a factor with number of levels equal to 'length(x)' and
### length equal to 'sum(elementLengths(x))'. 
unsplit_list_of_XVectorList <- function(classname, x, f)
{
    ans <- XVectorList(classname, length(f))
    unlisted_x <- do.call(c, unname(x))
    idx <- unname(split(seq_len(length(f)), f))
    ans[unlist(idx)] <- unlisted_x
    ans
}

