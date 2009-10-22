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
### Functions to help migrating from the "start/end" to the "shift/width"
### interface (remove when migration is over).
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Argument checking.
###

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
### TODO: Support non-integer weights in coverage().
coverage.normargWeight <- function(weight, nseq)
{
    weight <- normargWeight(weight, nseq)
    if (!is.integer(weight))
        weight <- as.integer(weight)
    weight
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods.
###

setMethod("coverage", "numeric",
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
        if (!is.integer(x))
            x <- as.integer(x)
        if (any(is.na(x)))
            stop("'x' contains NAs")
        sx <- x + shift
        if (is.null(width)) {
            width <- max(sx)
            ii <- which(1L <= sx)
        } else {
            ii <- which(1L <= sx & sx <= width)
        }
        if (width <= 0L)  # could be < 0 now if supplied width was NULL
            return(Rle())
        ## Restrict 'sx' (i.e. keep >= 1 and <= width values only)
        rsx <- sx[ii]
        rw <- weight[ii]
        Rle(sapply(seq_len(width), function(i) sum(rw[rsx == i])))
    }
)

.IRanges.coverage <- function(x, width, weight)
{
    .Call("IRanges_coverage", x, weight, width, PACKAGE="IRanges")
}

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
            ## By keeping all ranges, 'rsx' and 'weight' remain of the same
            ## length and the pairing between their elements is preserved.
            rsx <- restrict(sx, start=1L, keep.all.ranges=TRUE)
        } else {
            rsx <- restrict(sx, start=1L, end=width, keep.all.ranges=TRUE)
        }
        if (width <= 0L)  # could be < 0 now if supplied width was NULL
            return(Rle())
        .IRanges.coverage(rsx, width, weight)
    }
)

setMethod("coverage", "Views",
    function(x, start=NA, end=NA, shift=0L, width=NULL, weight=1L)
    {
        if (coverage.isCalledWithStartEndInterface(start, end, shift, width)) {
            ## From here, 'start' and 'end' cannot be single NAs
            shift <- coverage.getShift0FromStartEnd(start)
        }
        if (is.null(width))
            width <- length(subject(x)) + max(shift)
        coverage(as(x, "IRanges"), start=start, end=end, shift=shift, width=width, weight=weight)
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
            width <- width(mymasks)
        if (width <= 0L)  # should never be < 0
            return(Rle())
        ans <- new2("Rle", values=0L, lengths=width, check=FALSE)
        for (i in seq_len(length(x))) {
            nir <- x[[i]]
            if (isEmpty(nir))
                next()
            snir <- shift(nir, shift[i])
            rsnir <- restrict(snir, start=1L, end=width)
            ans <- ans + .IRanges.coverage(rsnir, width, weight[i])
        }
        ans
    }
)


setMethod("coverage", "RangesList",
    function(x,
             start = structure(rep(list(NA), length(x)), names = names(x)),
             end = structure(rep(list(NA), length(x)), names = names(x)),
             shift = structure(rep(list(0L), length(x)), names = names(x)),
             width = structure(rep(list(NULL), length(x)), names = names(x)),
             weight = structure(rep(list(1L), length(x)), names = names(x)))
    {
        indices <- names(x)
        if (is.null(indices))
            indices <- seq_len(length(x))
        else
            names(indices) <- indices
        newSimpleList("SimpleRleList",
                      lapply(indices,
                             function(i) {
                                 coverage(as(x[[i]], "IRanges"),
                                         start = start[[i]], end = end[[i]],
                                         shift = shift[[i]], width = width[[i]],
                                         weight = weight[[i]])
                             }),
                      metadata = metadata(x),
                      elementMetadata = elementMetadata(x))
    }
)

setMethod("coverage", "RangedData",
    function(x,
             start = structure(rep(list(NA), length(x)), names = names(x)),
             end = structure(rep(list(NA), length(x)), names = names(x)),
             shift = structure(rep(list(0L), length(x)), names = names(x)),
             width = structure(rep(list(NULL), length(x)), names = names(x)),
             weight = structure(rep(list(1L), length(x)), names = names(x)))
    {
        ranges <- ranges(x)
        if (length(metadata(x)) > 0)
            metadata(ranges) <- metadata(x)
        if (!is.null(elementMetadata(x)))
            elementMetadata(x) <- elementMetadata(x)
        varnames <- colnames(x)
        if (isSingleString(start) && (start %in% varnames))
            start <- values(x)[, start]
        if (isSingleString(end) && (end %in% varnames))
            end <- values(x)[, end]
        if (isSingleString(shift) && (shift %in% varnames))
            shift <- values(x)[, shift]
        if (isSingleString(width) && (width %in% varnames))
            width <- values(x)[, width]
        if (isSingleString(weight) && (weight %in% varnames))
            weight <- values(x)[, weight]
        coverage(ranges, start = start, end = end, shift = shift,
                 width = width, weight = weight)
    }
)
