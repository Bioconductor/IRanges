### =========================================================================
### coverage()
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### .IRanges.coverage() and .CompressedIRangesList.coverage()
###
### These 2 internal helpers are the workhorses behind most "coverage"
### methods. All the hard work is almost entirely performed at the C level.
### Only some argument checking/normalization plus the "folding" of the
### result are performed in R.
###

.fold_and_truncate_coverage <- function(cvg, circle.length, width)
{
    cvg <- fold(cvg, circle.length)
    if (is.na(width))
        return(cvg)
    head(cvg, n=width)
}

### Returns an Rle object.
.IRanges.coverage <- function(x,
                              shift=0L, width=NULL,
                              weight=1L, circle.length=NA,
                              method=c("auto", "sort", "hash"))
{
    ## Check 'x'.
    if (!is(x, "IRanges"))
        stop("'x' must be an IRanges object")

    ## Check 'shift' at the C level.

    ## Check 'width'.
    if (is.null(width)) {
        width <- NA_integer_
    } else if (!isSingleNumberOrNA(width)) {
        stop("'width' must be NULL or a single integer")
    } else if (!is.integer(width)) {
        width <- as.integer(width)
    }

    ## Check 'weight' at the C level.

    ## Check 'circle.length'.
    if (!isSingleNumberOrNA(circle.length))
        stop("'circle.length' must be a single integer")
    if (!is.integer(circle.length))
        circle.length <- as.integer(circle.length)

    ## Check 'method'.
    method <- match.arg(method)

    ## Ready to go...
    ans <- .Call2("IRanges_coverage", x,
                              shift, width,
                              weight, circle.length,
                              method,
                              PACKAGE="IRanges")

    if (is.na(circle.length))
        return(ans)
    .fold_and_truncate_coverage(ans, circle.length, width)
}

.check_arg_names <- function(arg, arg.label, x_names)
{
    arg_names <- names(arg)
    if (!(is.null(arg_names) || identical(arg_names, x_names)))
        stop("when '", arg.label, "' has names, ",
             "they must be identical to the names of 'x'")
}

### Returns a SimpleRleList object of the length of 'x'.
.CompressedIRangesList.coverage <- function(x,
                                            shift=0L, width=NULL,
                                            weight=1L, circle.length=NA,
                                            method=c("auto", "sort", "hash"))
{
    ## Check 'x'.
    if (!is(x, "CompressedIRangesList"))
        stop("'x' must be a CompressedIRangesList object")
    x_names <- names(x)

    ## Check 'shift'.
    if (!is.list(shift)) {
        if (!(is.numeric(shift) || is(shift, "List")))
            stop("'shift' must be a numeric vector or list-like object")
        shift <- as.list(shift)
    }
    .check_arg_names(shift, "shift", x_names)

    ## Check 'width'.
    if (is.null(width)) {
        width <- NA_integer_
    } else if (!is.numeric(width)) {
        stop("'width' must be NULL or an integer vector")
    } else if (!is.integer(width)) {
        width <- as.integer(width)
    }
    .check_arg_names(width, "width", x_names)

    ## Check 'weight'.
    if (!is.list(weight)) {
        if (!(is.numeric(weight) || is(weight, "List")))
            stop("'weight' must be a numeric vector or list-like object")
        weight <- as.list(weight)
    }
    .check_arg_names(weight, "weight", x_names)

    ## Check 'circle.length'.
    if (identical(circle.length, NA)) {
        circle.length <- NA_integer_
    } else if (!is.numeric(circle.length)) {
        stop("'circle.length' must be an integer vector")
    } else if (!is.integer(circle.length)) {
        circle.length <- as.integer(circle.length)
    }
    .check_arg_names(circle.length, "circle.length", x_names)

    ## Check 'method'.
    method <- match.arg(method)

    ## Ready to go...
    ans_listData <- .Call2("CompressedIRangesList_coverage", x,
                              shift, width,
                              weight, circle.length,
                              method,
                              PACKAGE="IRanges")

    ## "Fold" the coverage vectors in 'ans_listData' associated with a
    ## circular sequence.
    ## Note that the C code should have raised an error or warning already if
    ## the length of 'circle.length' or 'width' didn't allow proprer recycling
    ## to the length of 'x'. So using silent 'rep( , length.out=length(x))' is
    ## safe.
    circle.length <- rep(circle.length, length.out=length(x))
    fold_idx <- which(!is.na(circle.length))
    if (length(fold_idx) != 0L) {
        width <- rep(width, length.out=length(x))
        ## Because we "fold" the coverage vectors in an lapply() loop, it will
        ## be inefficient if 'x' has a lot of list elements associated with a
        ## circular sequence.
        ans_listData[fold_idx] <- lapply(fold_idx,
            function(i)
                .fold_and_truncate_coverage(ans_listData[[i]],
                                            circle.length[i],
                                            width[i]))
    }

    names(ans_listData) <- names(x)
    newList("SimpleRleList",
            ans_listData,
            metadata=metadata(x),
            mcols=mcols(x))
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### coverage() generic and methods.
###

setGeneric("coverage", signature="x",
    function(x, shift=0L, width=NULL, weight=1L, ...)
        standardGeneric("coverage")
)

setMethod("coverage", "Ranges",
    function(x, shift=0L, width=NULL, weight=1L,
                method=c("auto", "sort", "hash"))
    {
        if (isSingleString(weight)) {
            x_mcols <- mcols(x)
            if (!is(x_mcols, "DataTable")
             || sum(colnames(x_mcols) == weight) != 1L)
                stop("'mcols(x)' has 0 or more than 1 \"",
                     weight, "\" columns")
            weight <- x_mcols[[weight]]
        }
        .IRanges.coverage(as(x, "IRanges"),
                          shift=shift, width=width, weight=weight,
                          method=method)
    }
)

setMethod("coverage", "Views",
    function(x, shift=0L, width=NULL, weight=1L,
                method=c("auto", "sort", "hash"))
    {
        if (is.null(width))
            width <- length(subject(x))
        coverage(as(x, "IRanges"),
                 shift=shift,
                 width=width,
                 weight=weight,
                 method=method)
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
        .CompressedIRangesList.coverage(as(x, "CompressedIRangesList"),
                                        shift=shift, width=width,
                                        weight=weight,
                                        method=method)
    }
)

setMethod("coverage", "RangedData",
    function(x, shift=0L, width=NULL, weight=1L,
                method=c("auto", "sort", "hash"))
    {
        x_ranges <- ranges(x)
        if (length(metadata(x)) > 0)
            metadata(x_ranges) <- metadata(x)
        varnames <- colnames(x)
        if (isSingleString(shift) && (shift %in% varnames))
            shift <- values(x)[, shift]
        if (isSingleString(width) && (width %in% varnames))
            width <- values(x)[, width]
        if (isSingleString(weight) && (weight %in% varnames))
            weight <- values(x)[, weight]
        coverage(x_ranges, shift=shift, width=width, weight=weight,
                           method=method)
    }
)

