### =========================================================================
### The IRanges constructor
### -------------------------------------------------------------------------
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The SEW0 interface: start=NULL/end=NULL/width=NULL
###

.normargSEW0 <- function(x, argname)
{
    if (is.null(x))
        return(integer())
    if (!is.numeric(x) && !(is.atomic(x) && all(is.na(x))))
        stop("'", argname, "' must be a numeric vector (or NULL)")
    if (!is.integer(x))
        x <- as.integer(x)
    x
}

### Some of the functions that support the SEW0 interface: IRanges(), Views(),
### etc...
solveUserSEW0 <- function(start=NULL, end=NULL, width=NULL)
{
    start <- .normargSEW0(start, "start")
    end <- .normargSEW0(end, "end")
    width <- .normargSEW0(width, "width")
    L1 <- length(start)
    L2 <- length(end)
    L3 <- length(width)
    L123 <- c(L1, L2, L3)
    max123 <- max(L123)
    ## We want IRanges(start=integer(0), width=5) and
    ## IRanges(end=integer(0), width=5) to work and return an empty IRanges
    ## object.
    if (max123 == 0L || L1 == 0L && L2 == 0L && L3 == 1L)
        return(new("IRanges"))
    ## Recycle start/end/width.
    if (L1 < max123) {
        if (L1 == 0L)
            start <- rep.int(NA_integer_, max123)
        else
            start <- S4Vectors:::recycleVector(start, max123)
    }
    if (L2 < max123) {
        if (L2 == 0L)
            end <- rep.int(NA_integer_, max123)
        else
            end <- S4Vectors:::recycleVector(end, max123)
    }
    if (L3 < max123) {
        if (L3 == 0L)
            width <- rep.int(NA_integer_, max123)
        else
            width <- S4Vectors:::recycleVector(width, max123)
    }
    .Call2("solve_user_SEW0", start, end, width, PACKAGE="IRanges")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The safe and user-friendly "IRanges" constructor.
###

IRanges <- function(start=NULL, end=NULL, width=NULL, names=NULL)
{
    if (is(start, "Ranges")) {
        if (!is.null(end) || !is.null(width))
            stop("'end' and 'width' must be NULLs ",
                 "when 'start' is a Ranges object")
        ans <- new2("IRanges", start=start(start), width=width(start),
                    NAMES=names, check=FALSE)
        return(ans)
    }
    if ((is.logical(start) && !all(is.na(start))) || is(start, "Rle")) {
        if (is(start, "Rle") && !is.logical(runValue(start)))
            stop("'start' is an Rle, but not a logical Rle object")
        if (!is.null(end) || !is.null(width))
            stop("'end' and 'width' must be NULLs when 'start' is a logical ",
                 "vector or logical Rle")
        ## The returned IRanges instance is guaranteed to be normal.
        ans <- as(start, "IRanges")
        names(ans) <- names
        return(ans)
    }
    ans <- solveUserSEW0(start=start, end=end, width=width)
    names(ans) <- names
    return(ans)
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

    .Call2("solve_user_SEW",
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

