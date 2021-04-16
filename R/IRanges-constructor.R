### =========================================================================
### The IRanges constructor
### -------------------------------------------------------------------------
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Two low-level helpers
###
### Input can contain NAs. Output must be an unnamed integer vector.
###

.start_as_unnamed_integer <- function(start, what="a start")
{
    if (is.integer(start))
        return(unname(start))
    old_warn <- getOption("warn")
    options(warn=2L)
    on.exit(options(warn=old_warn))
    start <- try(as.integer(start), silent=TRUE)
    if (inherits(start, "try-error"))
        stop(wmsg("each range must have ", what, " that ",
                  "is < 2^31 and > - 2^31"))
    start
}

.width_as_unnamed_integer <- function(width, msg="a non-negative width")
{
    if (any(width < 0, na.rm=TRUE))
        stop(wmsg("each range must have ", msg))
    if (is.integer(width))
        return(unname(width))
    old_warn <- getOption("warn")
    options(warn=2L)
    on.exit(options(warn=old_warn))
    width <- try(as.integer(width), silent=TRUE)
    if (inherits(width, "try-error"))
        stop(wmsg("each range must have a width that is < 2^31"))
    width
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Low-level IRanges constructors
###

.new_empty_IRanges <- function() new2("IRanges", check=FALSE)

.new_IRanges_from_start_end <- function(start, end)
{
    if (!is.numeric(start) || !is.numeric(end))
        stop(wmsg("'start' and 'end' must be numeric vectors"))
    if (anyNA(start) || anyNA(end))
        stop(wmsg("'start' and 'end' cannot contain NAs"))
    if (length(start) == 0L || length(end) == 0L)
        return(.new_empty_IRanges())
    start <- .start_as_unnamed_integer(start)
    end   <- .start_as_unnamed_integer(end, what="an end")
    ## We want to perform this operation in "double" space rather
    ## than in "integer" space so we use 1.0 instead of 1L.
    width <- 1.0 + end - start
    width <- .width_as_unnamed_integer(width,
                 msg="an end that is greater or equal to its start minus one")
    start <- S4Vectors:::recycleVector(start, length(width))
    new2("IRanges", start=start, width=width, check=FALSE)
}

.new_IRanges_from_start_width <- function(start, width)
{
    if (!is.numeric(start) || !is.numeric(width))
        stop(wmsg("'start' and 'width' must be numeric vectors"))
    if (anyNA(start) || anyNA(width))
        stop(wmsg("'start' and 'width' cannot contain NAs"))
    if (length(start) == 0L || length(width) == 0L)
        return(.new_empty_IRanges())
    start <- .start_as_unnamed_integer(start)
    width <- .width_as_unnamed_integer(width)
    ## We want to perform this operation in "double" space rather
    ## than in "integer" space so we use -1.0 instead of -1L.
    end <- -1.0 + start + width
    end <- .start_as_unnamed_integer(end, what="an end")
    start <- S4Vectors:::recycleVector(start, length(end))
    width <- S4Vectors:::recycleVector(width, length(end))
    new2("IRanges", start=start, width=width, check=FALSE)
}

.new_IRanges_from_end_width <- function(end, width)
{
    if (!is.numeric(end) || !is.numeric(width))
        stop(wmsg("'end' and 'width' must be numeric vectors"))
    if (anyNA(end) || anyNA(width))
        stop(wmsg("'end' and 'width' cannot contain NAs"))
    if (length(end) == 0L || length(width) == 0L)
        return(.new_empty_IRanges())
    end   <- .start_as_unnamed_integer(end, what="an end")
    width <- .width_as_unnamed_integer(width)
    ## We want to perform this operation in "double" space rather
    ## than in "integer" space so we use 1.0 instead of 1L.
    start <- 1.0 + end - width
    start <- .start_as_unnamed_integer(start)
    start <- suppressWarnings(as.integer(start))
    width <- S4Vectors:::recycleVector(width, length(start))
    new2("IRanges", start=start, width=width, check=FALSE)
}

.is_numeric_or_NAs <- function(x)
{
    is.numeric(x) || is.logical(x) && all(is.na(x))
}

.solve_start_width_end <- function(start, end, width)
{
    if (!.is_numeric_or_NAs(start)
     || !.is_numeric_or_NAs(end)
     || !.is_numeric_or_NAs(width))
        stop(wmsg("'start', 'end', and 'width', must be numeric vectors"))
    L1 <- length(start)
    L2 <- length(end)
    L3 <- length(width)
    if (min(L1, L2, L3) == 0L)
        return(.new_empty_IRanges())
    if (is.logical(start)) {
        start <- as.integer(start)
    } else {
        start <- .start_as_unnamed_integer(start)
    }
    if (is.logical(end)) {
        end <- as.integer(end)
    } else {
        end <- .start_as_unnamed_integer(end, what="an end")
    }
    if (is.logical(width)) {
        width <- as.integer(width)
    } else {
        width <- .width_as_unnamed_integer(width)
    }
    ans_len <- max(L1, L2, L3)
    start <- S4Vectors:::recycleVector(start, ans_len)
    end   <- S4Vectors:::recycleVector(end, ans_len)
    width <- S4Vectors:::recycleVector(width, ans_len)
    .Call2("C_solve_start_width_end", start, end, width, PACKAGE="IRanges")
}

.new_IRanges <- function(start=NULL, end=NULL, width=NULL)
{
    start_is_null <- is.null(start)
    end_is_null <- is.null(end)
    width_is_null <- is.null(width)
    nb_of_nulls <- sum(start_is_null, end_is_null, width_is_null)
    if (nb_of_nulls == 3L)
        return(.new_empty_IRanges())
    if (nb_of_nulls == 2L)
        stop(wmsg("at least two of the 'start', 'end', and 'width' ",
                  "arguments must be supplied"))
    if (width_is_null)
        return(.new_IRanges_from_start_end(start, end))
    if (end_is_null)
        return(.new_IRanges_from_start_width(start, width))
    if (start_is_null)
        return(.new_IRanges_from_end_width(end, width))
    .solve_start_width_end(start, end, width)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### High-level IRanges constructor
###

IRanges <- function(start=NULL, end=NULL, width=NULL, names=NULL, ...)
{
    mcols <- DataFrame(..., check.names=FALSE)

    if (!is.null(start) && is.null(end) && is.null(width)) {
        ans <- as(start, "IRanges")
    } else {
        ans <- .new_IRanges(start=start, end=end, width=width)
    }

    if (!is.null(names))
        names(ans) <- names
    if (length(mcols) != 0L)
        mcols(ans) <- mcols
    ans
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The SEW interface: start=NA/end=NA/width=NA
###
### Some of the functions that support the SEW interface: narrow(),
### XVector::subseq(), XVector::xvcopy(), Biostrings::BStringSet() (and
### family), BSgenome::getSeq(), etc...
###

.normargSEW <- function(x, argname)
{
    if (!S4Vectors:::isNumericOrNAs(x))
        stop("'", argname, "' must be a vector of integers")
    if (!is.integer(x))
        x <- as.integer(x)
    x
}

### Use of 'rep.refwidths=TRUE' is supported only when 'refwidths' is of
### length 1.
### If 'rep.refwidths=FALSE' (the default) then 'start', 'end' and 'width'
### are recycled to 'length(refwidths)' (it's an error if one of them is
### longer than 'refwidths'). Otherwise, 'refwidths' is replicated L times
### where L is the length of the longest of 'start', 'end' and 'width'.
### The returned value is an IRanges object of the same length as 'refwidths'
### (after replication if 'rep.refwidths=TRUE').
solveUserSEW <- function(refwidths, start=NA, end=NA, width=NA,
                         rep.refwidths=FALSE,
                         translate.negative.coord=TRUE,
                         allow.nonnarrowing=FALSE)
{
    if (!is.numeric(refwidths))
        stop("'refwidths' must be a vector of integers")
    if (!is.integer(refwidths))
        refwidths <- as.integer(refwidths)

    start <- .normargSEW(start, "start")
    end <- .normargSEW(end, "end")
    width <- .normargSEW(width, "width")
    ## From here, 'refwidths', 'start', 'end' and 'width' are guaranteed to be
    ## integer vectors. NAs in 'start', 'end' and 'width' are OK but not in
    ## 'refwidths' so this should be checked at the C level.

    if (!isTRUEorFALSE(rep.refwidths))
        stop("'rep.refwidths' must be TRUE or FALSE")

    if (!isTRUEorFALSE(translate.negative.coord))
        stop("'translate.negative.coord' must be TRUE or FALSE")
    if (!isTRUEorFALSE(allow.nonnarrowing))
        stop("'allow.nonnarrowing' must be TRUE or FALSE")

    Lsew <- c(length(start), length(end), length(width))
    maxLsew <- max(Lsew)
    minLsew <- min(Lsew)
    if (minLsew == 0L && maxLsew > 1L)
        stop("'start', 'end' and 'width' cannot mix zero-length ",
             "and longer-than-one vectors")

    ## Check 'start', 'end', and 'width' *without* recycling them. Recycling
    ## is done at the C level.
    if (rep.refwidths) {
        if (length(refwidths) != 1L)
            stop("'rep.refwidths=TRUE' can be used only when 'refwidths' ",
                 "is of length 1")
        ## 'ans_len' is the length of the longest of 'start', 'end'
        ## and 'width'.
        if (minLsew == 0L) {
            ans_len <- 0L
        } else {
            ans_len <- maxLsew
        }
        refwidths <- rep.int(refwidths, ans_len)
    } else {
        ans_len <- length(refwidths)
        if (ans_len == 0L) {
            if (maxLsew > 1L)
                stop("'start', 'end' or 'width' is longer than 'refwidths'")
        } else {
            if (minLsew == 0L)
                stop("cannot recycle empty 'start', 'end' or 'width'")
            if (maxLsew > ans_len)
                stop("'start', 'end' or 'width' is longer than 'refwidths'")
        }
    }

    .Call2("C_solve_user_SEW",
           refwidths, start, end, width,
           translate.negative.coord, allow.nonnarrowing,
           PACKAGE="IRanges")
}

### Returns an IRanges instance of length 1. Not exported.
solveUserSEWForSingleSeq <- function(x_length, start=NA, end=NA, width=NA)
{
    solved_SEW <-
      try(solveUserSEW(x_length, start=start, end=end, width=width),
          silent = TRUE)
    if (is(solved_SEW, "try-error"))
        stop("Invalid sequence coordinates.\n",
             "  Please make sure the supplied 'start', 'end' and 'width' arguments\n",
             "  are defining a region that is within the limits of the sequence.")
    solved_SEW
}

