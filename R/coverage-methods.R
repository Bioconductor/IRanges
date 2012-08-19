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

.normargWidth <- function(width, nseq)
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

### Can be used by any "coverage" method returning an RleList to transform
### their 'shift', 'width', and 'weight' arguments into lists of the same
### length as the returned RleList.
### 'refnames' must be either NULL or a character vector of length
### 'length.out'.
normarg_for_multiple_coverage <- function(arg, argname, length.out,
                                          refnames, default.argval)
{
    if (is.null(arg)) {
        arg <- list(NULL)
    } else if (is.numeric(arg)) {
        arg <- as.list(arg)
    } else if (!is.list(arg) && !is(arg, "List")) {
        stop("'arg' must be NULL, or a numeric vector, or a list-like object")
    }
    arg_names <- names(arg)
    if (is.null(arg_names)) {
        arg <- recycleArg(arg, argname, length.out)
        return(arg)
    }
    if (is.null(refnames))
        stop("'", argname, "' cannot be named when 'x' is not")
    arg2ref <- match(arg_names, refnames)
    if (any(is.na(arg2ref)))
        stop("'", argname, "' has names not in 'names(x)'")
    if (anyDuplicated(arg_names))
        stop("'", argname, "' has duplicated names")
    arg2 <- rep.int(as(list(default.argval), class(arg)), length.out)
    arg2[arg2ref] <- arg
    return(arg2)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods.
###

setMethod("coverage", "numeric",
    function(x, shift=0L, width=NULL, weight=1L)
    {
        shift <- recycleIntegerArg(shift, "shift", length(x))
        width <- .normargWidth(width, length(x))
        weight <- recycleIntegerArg(weight, "weight,", length(x))
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

.select.method <- function(x, width)
{
    ## Based on empirical observation.
    if (length(x) <= 0.25 * width)
        return("sort")
    return("hash")
}

.Ranges.integer.coverage <- function(x, width, weight, method="auto")
{
    if (method == "auto")
        method <- .select.method(x, width)
    .Call2("Ranges_integer_coverage",
           start(x), width(x), width, weight, method,
           PACKAGE="IRanges")
}

.Ranges.numeric.coverage <- function(x, width, weight, method="auto")
{
    if (method == "auto")
        method <- .select.method(x, width)
    .Call2("Ranges_numeric_coverage",
           start(x), width(x), width, weight, method,
           PACKAGE="IRanges")
}

.Ranges.coverage <- function(x, width, weight, method="auto")
{
    weight_type <- typeof(weight)
    FUN <- switch(weight_type,
        "integer"=.Ranges.integer.coverage,
        "double"=.Ranges.numeric.coverage,
        stop("type of 'weight' must be integer or double, got ", weight_type))
    FUN(x, width, weight, method=method)
}

setMethod("coverage", "IRanges",
    function(x, shift=0L, width=NULL, weight=1L,
             method=c("auto", "sort", "hash"))
    {
        method <- match.arg(method)
        sx <- shift(x, shift)
        width <- .normargWidth(width, length(x))
        if (isSingleString(weight)) {
            x_mcols <- mcols(x)
            if (!is(x_mcols, "DataTable")
             || sum(colnames(x_mcols) == weight) != 1L)
                stop("'mcols(x)' has 0 or more than 1 \"",
                     weight, "\" columns")
            weight <- x_mcols[[weight]]
        } 
        weight <- recycleNumericArg(weight, "weight", length(x))
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
        .Ranges.coverage(rsx, width, weight, method)
    }
)

setMethod("coverage", "Views",
    function(x, shift=0L, width=NULL, weight=1L,
             method=c("auto", "sort", "hash"))
    {
        method <- match.arg(method)
        if (is.null(width))
            width <- length(subject(x)) + max(shift)
        coverage(as(x, "IRanges"),
                 shift=shift,
                 width=width,
                 weight=weight,
                 method=method)
    }
)

### TODO: Implementation below could be made more efficient and simpler by
### just calling coverage() on the single IRanges object resulting from
### unlisting 'x' ('shift' and 'weight' must be modified consequently).
setMethod("coverage", "MaskCollection",
    function(x, shift=0L, width=NULL, weight=1L,
             method=c("auto", "sort", "hash"))
    {
        method <- match.arg(method)
        shift <- recycleIntegerArg(shift, "shift", length(x))
        width <- .normargWidth(width, length(x))
        if (isSingleString(weight)) {
            x_mcols <- mcols(x)
            if (!is(x_mcols, "DataTable")
             || sum(colnames(x_mcols) == weight) != 1L)
                stop("'mcols(x)' has 0 or more than 1 \"",
                     weight, "\" columns")
            weight <- x_mcols[[weight]]
        } 
        weight <- recycleNumericArg(weight, "weight", length(x))
        if (is.null(width))
            width <- width(x)
        if (width <= 0L)  # should never be < 0
            return(Rle())
        ans <- new2("Rle", values=0L, lengths=width, check=FALSE)
        for (i in seq_len(length(x))) {
            nir <- x[[i]]
            if (isEmpty(nir))
                next()
            snir <- shift(nir, shift[i])
            rsnir <- restrict(snir, start=1L, end=width)
            ans <- ans + .Ranges.coverage(rsnir, width, weight[i], method)
        }
        ans
    }
)

setMethod("coverage", "RangesList",
    function(x, shift=0L, width=NULL, weight=1L,
             method=c("auto", "sort", "hash"))
    {
        x_mcols <- mcols(x)
        x_mcolnames <- colnames(x_mcols)
        if (isSingleString(shift)) {
            if (!(shift %in% x_mcolnames))
                stop("the string supplied for 'shift' (\"", shift, "\")",
                     "is not a valid metadata column name of 'x'")
            shift <- x_mcols[[shift]]
        }
        if (isSingleString(width)) {
            if (!(width %in% x_mcolnames))
                stop("the string supplied for 'width' (\"", width, "\")",
                     "is not a valid metadata column name of 'x'")
            width <- x_mcols[[width]]
        }
        if (isSingleString(weight)) {
            if (!(weight %in% x_mcolnames))
                stop("the string supplied for 'weight' (\"", weight, "\")",
                     "is not a valid metadata column name of 'x'")
            weight <- x_mcols[[weight]]
        }
        x_len <- length(x)
        shift <- normarg_for_multiple_coverage(shift, "shift", x_len,
                                               names(x), 0L)
        width <- normarg_for_multiple_coverage(width, "width", x_len,
                                               names(x), NULL)
        weight <- normarg_for_multiple_coverage(weight, "weight", x_len,
                                               names(x), 1L)
        method <- match.arg(method)
        ans_listData <- lapply(seq_len(x_len),
                               function(i) {
                                   coverage(as(x[[i]], "IRanges"),
                                            shift=shift[[i]],
                                            width=width[[i]],
                                            weight=weight[[i]],
                                            method=method)
                               })
        names(ans_listData) <- names(x)
        newList("SimpleRleList",
                ans_listData,
                metadata=metadata(x),
                mcols=mcols(x))
    }
)

setMethod("coverage", "RangedData",
    function(x,
             shift = structure(rep(list(0L), length(x)), names = names(x)),
             width = structure(rep(list(NULL), length(x)), names = names(x)),
             weight = structure(rep(list(1L), length(x)), names = names(x)),
             method = c("auto", "sort", "hash"))
    {
        method <- match.arg(method)
        ranges <- ranges(x)
        if (length(metadata(x)) > 0)
            metadata(ranges) <- metadata(x)
        if (!is.null(mcols(x)))
            mcols(x) <- mcols(x)
        varnames <- colnames(x)
        if (isSingleString(shift) && (shift %in% varnames))
            shift <- values(x)[, shift]
        if (isSingleString(width) && (width %in% varnames))
            width <- values(x)[, width]
        if (isSingleString(weight) && (weight %in% varnames))
            weight <- values(x)[, weight]
        coverage(ranges, shift = shift, width = width, weight = weight, method = method)
    }
)
