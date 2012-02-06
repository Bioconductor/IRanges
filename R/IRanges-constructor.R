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

### Some of the functions that support the SEW0 interface: IRanges(),
### seqselect(), Views(), etc...
solveUserSEW0 <- function(start=NULL, end=NULL, width=NULL)
{
    start <- .normargSEW0(start, "start")
    end <- .normargSEW0(end, "end")
    width <- .normargSEW0(width, "width")
    l1 <- length(start)
    l2 <- length(end)
    l3 <- length(width)
    max123 <- max(l1, l2, l3)
    if (max123 == 0L)
        return(new("IRanges"))
    ## Recycle start/end/width.
    if (l1 < max123) {
        if (l1 == 0L)
            start <- rep.int(NA_integer_, max123)
        else
            start <- recycleVector(start, max123)
    }
    if (l2 < max123) {
        if (l2 == 0L)
            end <- rep.int(NA_integer_, max123)
        else
            end <- recycleVector(end, max123)
    }
    if (l3 < max123) {
        if (l3 == 0L)
            width <- rep.int(NA_integer_, max123)
        else
            width <- recycleVector(width, max123)
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
### Some of the functions that support the SEW interface: narrow(), subseq(),
### xvcopy(), Biostrings::BStringSet() (and family), BSgenome::getSeq(), etc...
###

.normargSEW <- function(x, argname)
{
    if (!isNumericOrNAs(x))
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
    if (!isTRUEorFALSE(rep.refwidths))
        stop("'rep.refwidths' must be TRUE or FALSE")
    if (rep.refwidths && length(refwidths) != 1L)
        stop("use 'rep.refwidths=TRUE' only when 'refwidths' is of length 1")
    start <- .normargSEW(start, "start")
    end <- .normargSEW(end, "end")
    width <- .normargSEW(width, "width")
    ## From here, 'refwidths', 'start', 'end' and 'width' are guaranteed to be
    ## integer vectors. NAs in 'start', 'end' and 'width' are OK but not in
    ## 'refwidths' so this needs to be checked by C function solve_user_SEW().
    Lsew <- c(length(start), length(end), length(width))
    maxLsew <- max(Lsew)
    if (min(Lsew) == 0L && maxLsew != 0L)
        stop("'start', 'end' and 'width' cannot be a mix of zero-length ",
             "and non zero-length vectors")
    if (rep.refwidths) {
        refwidths <- rep.int(refwidths, maxLsew)
    } else {
        if (maxLsew > length(refwidths) && maxLsew > 1L)
            stop("'start', 'end' or 'width' is longer than 'refwidths'")
        if (length(refwidths) != 0L && maxLsew == 0L)
            stop("cannot recycle empty 'start', 'end' and 'width'")
    }
    if (!isTRUEorFALSE(translate.negative.coord))
        stop("'translate.negative.coord' must be TRUE or FALSE")
    if (!isTRUEorFALSE(allow.nonnarrowing))
        stop("'allow.nonnarrowing' must be TRUE or FALSE")
    .Call2("solve_user_SEW",
          refwidths, start, end, width,
          translate.negative.coord, allow.nonnarrowing,
          PACKAGE="IRanges")
}

