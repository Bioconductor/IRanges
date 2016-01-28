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

    ## 'shift' will be checked at the C level.
    if (is(shift, "Rle"))
        shift <- S4Vectors:::decodeRle(shift)

    ## Check 'width'.
    if (is.null(width)) {
        width <- NA_integer_
    } else if (!isSingleNumberOrNA(width)) {
        stop("'width' must be NULL or a single integer")
    } else if (!is.integer(width)) {
        width <- as.integer(width)
    }

    ## 'weight' will be checked at the C level.
    if (is(weight, "Rle"))
        weight <- S4Vectors:::decodeRle(weight)

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

### Returns an ordinary list.
.normarg_shift_or_weight <- function(arg, arg.label)
{
    if (!is.list(arg)) {
        if (!(is.numeric(arg) ||
              (is(arg, "Rle") && is.numeric(runValue(arg))) ||
              is(arg, "List")))
            stop("'", arg.label, "' must be a numeric vector ",
                 "or a list-like object")
        arg <- as.list(arg)
    }
    if (length(arg) != 0L) {
        idx <- which(sapply(arg, is, "Rle"))
        if (length(idx) != 0L)
            arg[idx] <- lapply(arg[idx], S4Vectors:::decodeRle)
    }
    arg
}

.check_arg_names <- function(arg, arg.label, x_names, x_names.label)
{
    arg_names <- names(arg)
    if (!(is.null(arg_names) || identical(arg_names, x_names)))
        stop("when '", arg.label, "' has names, ",
             "they must be identical to ", x_names.label)
}

## Some packages like easyRNASeq or TEQC pass 'width' as a named list-like
## object where each list element is a single number, an NA, or a NULL, when
## calling coverage() on a RangesList or RangedData object. They do so because,
## for whatever reason, we've been supporting this for a while, and also
## because, in the case of the method for RangedData objects, the arg default
## for 'width' used to be such a list (a named list of NULLs in that case).
## However, it never really made sense to support a named list-like object for
## 'width', and it makes even less sense now that the signature of the method
## for RangedData objects has been modified (as of BioC 2.13) to use the same
## arg defaults as the coverage() generic and all other methods.
## TODO: Deprecate support for this. Preferred 'width' form: NULL or an integer
## vector. An that's it.
.unlist_width <- function(width, x_names, x_names.label)
{
    if (!identical(names(width), x_names))
        stop("when 'width' is a list-like object, it must be named ",
             "and its names must be identical to ", x_names.label)
    width_eltNROWS <- elementNROWS(width)
    if (!all(width_eltNROWS <= 1L))
        stop("when 'width' is a list-like object, each list element ",
             "should contain at most 1 element or be NULL")
    width[width_eltNROWS == 0L] <- NA_integer_
    setNames(unlist(width, use.names=FALSE), x_names)
}

### Returns a SimpleRleList object of the length of 'x'.
.CompressedIRangesList.coverage <- function(x,
                                            shift=0L, width=NULL,
                                            weight=1L, circle.length=NA,
                                            method=c("auto", "sort", "hash"),
                                            x_names.label="'x' names")
{
    ## Check 'x'.
    if (!is(x, "CompressedIRangesList"))
        stop("'x' must be a CompressedIRangesList object")
    x_names <- names(x)

    ## Check and normalize 'shift'.
    shift <- .normarg_shift_or_weight(shift, "shift")
    .check_arg_names(shift, "shift", x_names, x_names.label)

    ## Check and normalize 'width'.
    if (is.null(width)) {
        width <- NA_integer_
    } else {
        if (is.numeric(width)) {
            .check_arg_names(width, "width", x_names, x_names.label)
        } else if (is.list(width) || is(width, "List")) {
            width <- .unlist_width(width, x_names, x_names.label)
        } else {
            ## We purposedly omit to mention that 'width' can also be a named
            ## list-like object because this will be deprecated soon (this is
            ## why it's not documented in man/coverage-methods.Rd either).
            stop("'width' must be NULL or an integer vector")
        }
        if (!is.integer(width))
            width <- setNames(as.integer(width), names(width))
    }

    ## Check and normalize 'weight'.
    weight <- .normarg_shift_or_weight(weight, "weight")
    .check_arg_names(weight, "weight", x_names, x_names.label)

    ## Check and normalize 'circle.length'.
    if (identical(circle.length, NA)) {
        circle.length <- NA_integer_
    } else if (!is.numeric(circle.length)) {
        stop("'circle.length' must be an integer vector")
    } else if (!is.integer(circle.length)) {
        circle.length <- setNames(as.integer(circle.length),
                                  names(circle.length))
    }
    .check_arg_names(circle.length, "circle.length", x_names, x_names.label)

    ## Check and normalize 'method'.
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
    S4Vectors:::new_SimpleList_from_list("SimpleRleList", ans_listData,
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

        if (isSingleString(weight) && (weight %in% varnames))
            weight <- values(x)[, weight]

        coverage(x_ranges, shift=shift, width=width, weight=weight,
                           method=method)
    }
)

