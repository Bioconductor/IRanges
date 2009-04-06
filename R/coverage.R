### =========================================================================
### coverage()
### -------------------------------------------------------------------------
###
### NOTE: The interface of the coverage() generic is currently being migrated
### from "start/end" to "shift/width". Here is the roadmap:
### Starting with IRanges 1.1.58, coverage() arguments have changed from
###   coverage(x, start=NA, end=NA, ...)
### to
###   coverage(x, start=NA, end=NA, shift=0L, width=NULL, weight=1L, ...)
### In the near future, the start/end arguments will be dropped and the
### remaining arguments will be:
###   coverage(x, shift=0L, width=NULL, weight=1L, ...)
### The "shift/width" interface is more intuitive, more convenient and
### offers slighty more control than the "start/end" interface. In addition
### it's already used by Biostrings::consensusMatrix() and
### Biostrings::consensusString() which are both related to coverage() (see
### ?consensusMatrix).
### Also it makes sense to add the 'weight' argument to the generic (vs
### having it supported only by some methods) since weighting the
### elements in 'x' can be considered part of the concept of coverage
### in general.
###

setGeneric("coverage", signature="x",
    function(x, start=NA, end=NA, shift=0L, width=NULL, weight=1L, ...)
        standardGeneric("coverage")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Argument checking.
###

coverage.isCalledWithStartEndInterface <- function(start, end, shift, width)
{
    isSingleNA <- function(x) {is.atomic(x) && length(x) == 1 && is.na(x)}
    if (isSingleNA(start) && isSingleNA(end))
        return(FALSE)
    if (!identical(shift, 0L) || !is.null(width))
        stop("you cannot use both the \"start/end\" interface and\n",
             "  the \"shift/width\" interface when calling coverage()")
    warning("the signature of coverage() has changed.\n  Please use the ",
            "\"shift/width\" interface instead of the \"start/end\" interface.\n",
            "  See '?coverage'")
    if (isSingleNA(start) || isSingleNA(end))
        stop("when calling coverage() with the \"start/end\" interface,\n",
             "  both 'start' and 'end' must be specified")
    TRUE
}

coverage.getShift0FromStartEnd <- function(start)
{
    if (!isSingleNumber(start))
        stop("when specified, 'start' must be a single integer")
    if (!is.integer(start))
        start <- as.integer(start)
    1L - start
}

coverage.getWidthFromStartEnd <- function(end, shift0)
{
    if (!isSingleNumber(end))
        stop("when specified, 'end' must be a single integer")
    if (!is.integer(end))
        end <- as.integer(end)
    width <- end + shift0
    if (width < 0L)
        stop("when specified, 'end' must be >= 'start' - 1")
    width
}

coverage.normargWidth <- function(width, nseq)
{
    if (is.null(width)) {
        if (nseq == 0L)
            stop("'x' has no element and 'width' is NULL")
        return(width)
    }
    if (!isSingleNumber(width) || width < 0)
        stop("'width' must be NULL or a single non-negative integer")
    if (!is.integer(width))
        width <- as.integer(width)
    width
}

### Implements the same logic as normargShift().
coverage.normargWeight <- function(weight, nseq)
{
    if (!is.numeric(weight))
        stop("'weight' must be a vector of integers")
    if (!is.integer(weight))
        weight <- as.integer(weight)
    if (nseq == 0L) {
        weight <- integer()
    } else {
        if (length(weight) == 0L)
            stop("'weight' has no elements")
        if (length(weight) > nseq)
            stop("'weight' is longer than 'x'")
        if (any(is.na(weight)))
            stop("'weight' contains NAs")
        if (length(weight) < nseq)
            weight <- recycleVector(weight, nseq)
    }
    weight
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods.
###

setMethod("coverage", "IRanges",
    function(x, start=NA, end=NA, shift=0L, width=NULL, weight=1L)
    {
        if (coverage.isCalledWithStartEndInterface(start, end, shift, width)) {
            ## From here, 'start' and 'end' cannot be single NAs
            shift <- coverage.getShift0FromStartEnd(start)
            width <- coverage.getWidthFromStartEnd(end, shift)
        } else {
            width <- coverage.normargWidth(width, length(x))
        }
        #shift <- normargShift(shift, length(x))  # done by shift() below
        weight <- coverage.normargWeight(weight, length(x))
        sx <- shift(x, shift)
        if (is.null(width)) {
            width <- max(end(sx))
            rsx <- restrict(sx, start=1L)
        } else {
            rsx <- restrict(sx, start=1L, end=width)
        }
        .Call("IRanges_coverage", rsx, weight, order(start(rsx)), width, PACKAGE="IRanges")
    }
)

### TODO: Implementation below could be made more efficient and simpler by
### just calling coverage() on the single IRanges object resulting from
### unlisting 'x' ('shift' and 'weight' must be modified consequently).
setMethod("coverage", "MaskCollection",
    function(x, start=NA, end=NA, shift=0L, width=NULL, weight=1L)
    {
        if (coverage.isCalledWithStartEndInterface(start, end, shift, width)) {
            ## From here, 'start' and 'end' cannot be single NAs
            shift <- coverage.getShift0FromStartEnd(start)
            width <- coverage.getWidthFromStartEnd(end, shift)
        } else {
            width <- coverage.normargWidth(width, length(x))
        }
        shift <- normargShift(shift, length(x))
        weight <- coverage.normargWeight(weight, length(x))
        if (is.null(width))
            width <- max(sapply(seq_len(length(x)), function(i) {max(end(x[[i]])) + shift[i]}))
        ans <- new("Rle", values = 0L, lengths = width)
        for (i in seq_len(length(x))) {
            sx <- shift(x[[i]], shift[i])
            rsx <- restrict(sx, start=1L, end=width)
            ans <- ans +
              .Call("IRanges_coverage", rsx, weight[i], order(start(rsx)), width,
                    PACKAGE="IRanges")
        }
        ans
    }
)

