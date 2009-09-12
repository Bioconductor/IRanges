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
    .Call("solve_user_SEW0", start, end, width, PACKAGE="IRanges")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The safe and user-friendly "IRanges" constructor.
###

IRanges <- function(start=NULL, end=NULL, width=NULL, names=NULL)
{
    if (is(start, "Ranges")) {
        if (!is.null(end) || !is.null(width))
            stop("'end' and 'width' must be NULLs when 'start' is a Ranges object")
        names(start) <- names
        return(start)
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

.normargSEW <- function(x, argname)
{
    if (!isNumericOrNAs(x))
        stop("'", argname, "' must be a vector of integers")
    if (!is.integer(x))
        x <- as.integer(x)
    x
}

### Some of the functions that support the SEW interface: narrow(), subseq(),
### Biostrings::BStringSet() (and family), BSgenome::getSeq(), etc...
solveUserSEW <- function(refwidths, start=NA, end=NA, width=NA,
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
    ## 'refwidths' so this needs to be checked by C function solve_user_SEW().
    l1 <- length(start)
    l2 <- length(end)
    l3 <- length(width)
    min123 <- min(l1, l2, l3)
    max123 <- max(l1, l2, l3)
    if (length(refwidths) == 0L) {
        if (max123 != min123 || max123 > 1L)
            stop("'start', 'end' and 'width' must have the same length ",
                 "and it must be 0 or 1 when 'refwidths' is empty")
    } else {
        if (max123 > length(refwidths))
            stop("'start', 'end' or 'width' has more elements than 'refwidths'")
        if (min123 == 0L)
            stop("'start', 'end' or 'width' is empty but 'refwidths' is not")
    }
    if (!isTRUEorFALSE(translate.negative.coord))
        stop("'translate.negative.coord' must be TRUE or FALSE")
    if (!isTRUEorFALSE(allow.nonnarrowing))
        stop("'allow.nonnarrowing' must be TRUE or FALSE")
    .Call("solve_user_SEW",
          refwidths, start, end, width,
          translate.negative.coord, allow.nonnarrowing,
          PACKAGE="IRanges")
}

