### =========================================================================
### coverage()
### -------------------------------------------------------------------------
###

setGeneric("coverage", signature="x",
    function(x, shift=0L, width=NULL, weight=1L, ...)
        standardGeneric("coverage")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Argument checking.
###

coverage.normargWidth <- function(width, nseq)
{
    if (is.null(width)) {
        if (nseq == 0L)
            width <- 0L
    } else {
        if (!isSingleNumber(width) || width < 0)
            stop("'width' must be NULL or a single non-negative integer")
        if (!is.integer(width))
            width <- as.integer(width)
    }
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
    function(x, shift=0L, width=NULL, weight=1L)
    {
        shift <- normargShift(shift, length(x))
        width <- coverage.normargWidth(width, length(x))
        weight <- coverage.normargWeight(weight, length(x))
        if (!is.integer(x))
            x <- as.integer(x)
        if (anyMissing(x))
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
    function(x, shift=0L, width=NULL, weight=1L)
    {
        #shift <- normargShift(shift, length(x))  # done by shift() below
        width <- coverage.normargWidth(width, length(x))
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
    function(x, shift=0L, width=NULL, weight=1L)
    {
        if (is.null(width))
            width <- length(subject(x)) + max(shift)
        coverage(as(x, "IRanges"), shift=shift, width=width, weight=weight)
    }
)

### TODO: Implementation below could be made more efficient and simpler by
### just calling coverage() on the single IRanges object resulting from
### unlisting 'x' ('shift' and 'weight' must be modified consequently).
setMethod("coverage", "MaskCollection",
    function(x, shift=0L, width=NULL, weight=1L)
    {
        shift <- normargShift(shift, length(x))
        width <- coverage.normargWidth(width, length(x))
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
             shift = structure(rep(list(0L), length(x)), names = names(x)),
             width = structure(rep(list(NULL), length(x)), names = names(x)),
             weight = structure(rep(list(1L), length(x)), names = names(x)))
    {
        lx <- length(x)
        if (lx != 0 &&
            ((!is.list(shift) && !is(shift, "IntegerList")) ||
             length(shift) == 0))
            stop("'shift' must be a non-empty list of integers")
        if (length(shift) < lx)
            shift <- rep(shift, length.out = lx)
        if (lx != 0 &&
            ((!is.list(width) && !is(width, "IntegerList")) ||
             length(width) == 0))
            stop("'width' must be a non-empty list")
        if (length(width) < lx)
            width <- rep(width, length.out = lx)
        if (lx != 0 &&
            ((!is.list(weight) && !is(weight, "IntegerList")) ||
             length(weight) == 0))
            stop("'weight' must be a non-empty list of integers")
        if (length(weight) < lx)
            weight <- rep(weight, length.out = lx)
        indices <- names(x)
        if (is.null(indices)) {
            indices <- seq_len(lx)
        } else {
            if (is.null(names(shift)))
                names(shift) <- indices
            if (length(setdiff(names(shift), indices)) > 0)
                stop("'x' and 'shift' have mismatching names")
            if (is.null(names(width)))
                names(width) <- indices
            if (length(setdiff(names(width), indices)) > 0)
                stop("'x' and 'width' have mismatching names")
            if (is.null(names(weight)))
                names(weight) <- indices
            if (length(setdiff(names(weight), indices)) > 0)
                stop("'x' and 'weight' have mismatching names")
            names(indices) <- indices
        }
        newSimpleList("SimpleRleList",
                      lapply(indices,
                             function(i) {
                                 coverage(as(x[[i]], "IRanges"),
                                         shift = shift[[i]], width = width[[i]],
                                         weight = weight[[i]])
                             }),
                      metadata = metadata(x),
                      elementMetadata = elementMetadata(x))
    }
)

setMethod("coverage", "RangedData",
    function(x,
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
        if (isSingleString(shift) && (shift %in% varnames))
            shift <- values(x)[, shift]
        if (isSingleString(width) && (width %in% varnames))
            width <- values(x)[, width]
        if (isSingleString(weight) && (weight %in% varnames))
            weight <- values(x)[, weight]
        coverage(ranges, shift = shift, width = width, weight = weight)
    }
)
