### =========================================================================
### Utility functions for creating or modifying IRanges objects
### -------------------------------------------------------------------------
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "successiveIRanges" function.
###
### Note that the returned IRanges object is guaranteed to be normal in the
### following cases:
###   (a) when length(width) == 0
###   (b) when length(width) == 1 and width > 0
###   (c) when length(width) >= 2 and all(width > 0) and all(gapwidth > 0)
### However, the function doesn't try to turn the result into a NormalIRanges
### object.
###

successiveIRanges <- function(width, gapwidth=0, from=1)
{
    if (!is.numeric(width))
        stop("'width' must be an integer vector")
    if (length(width) == 0L)
        return(IRanges())
    if (!is.integer(width))
        width <- as.integer(width)  # this drops the names
    else if (!is.null(names(width)))
        names(width) <- NULL  # unname() used to be broken on 0-length vectors
    if (S4Vectors:::anyMissingOrOutside(width, 0L))
        stop("'width' cannot contain NAs or negative values")
    if (!is.numeric(gapwidth))
        stop("'gapwidth' must be an integer vector")
    if (!is.integer(gapwidth))
        gapwidth <- as.integer(gapwidth)
    if (S4Vectors:::anyMissing(gapwidth))
        stop("'gapwidth' cannot contain NAs")
    if (length(gapwidth) != length(width) - 1L) {
        if (length(gapwidth) != 1L)
            stop("'gapwidth' must a single integer or an integer vector ",
                 "with one less element than the 'width' vector")
        gapwidth <- rep.int(gapwidth, length(width) - 1L)
    }
    if (!isSingleNumber(from))
        stop("'from' must be a single integer")
    if (!is.integer(from))
        from <- as.integer(from)
    ans_start <- cumsum(width[-length(width)] + gapwidth)
    ans_start <- from + c(0L, ans_start)
    ## 'ans_start' could contain NAs in case of an integer overflow in
    ## cumsum(), hence the use of 'check=TRUE' here:
    new2("IRanges", start=ans_start, width=width, check=TRUE)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### slidingIRanges()
###

slidingIRanges <- function(len, width, shift = 1L) {
    start <- seq(1L, len-width, by=shift)
    end <- seq(width, len, by=shift)
    IRanges(start, end)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### breakInChunks()
###
### TODO: Should not be in IRanges-utils.R because it returns a
### PartitioningByEnd object, not an IRanges object. So move it to another
### file, e.g. to Partitioning-class.R. breakInChunks() is actually a
### specialized PartitioningByEnd constructor.
###

.normarg_totalsize <- function(totalsize)
{
    if (!isSingleNumber(totalsize))
        stop("'totalsize' must be a single integer")
    if (!is.integer(totalsize))
        totalsize <- as.integer(totalsize)
    if (totalsize < 0L)
        stop("'totalsize' cannot be negative")
    totalsize
}

.normarg_nchunk_or_chunksize <- function(nchunk, totalsize, what)
{
    if (!isSingleNumber(nchunk))
        stop("'", what, "' must be a single integer")
    if (!is.integer(nchunk))
        nchunk <- as.integer(nchunk)
    if (nchunk < 0L)
        stop("'", what, "' cannot be negative")
    if (nchunk == 0L && totalsize != 0L)
        stop("'", what, "' can be 0 only if 'totalsize' is 0")
    nchunk
}

breakInChunks <- function(totalsize, nchunk, chunksize)
{
    totalsize <- .normarg_totalsize(totalsize)
    if (!missing(nchunk)) {
        if (!missing(chunksize))
            stop("only one of 'nchunk' and 'chunksize' can be specified")
        ## All chunks will have more or less the same size, with the difference
        ## between smallest and biggest chunks guaranteed to be <= 1.
        nchunk <- .normarg_nchunk_or_chunksize(nchunk, totalsize,
                                               "nchunk")
        if (nchunk == 0L)
            return(PartitioningByEnd())
        chunksize <- totalsize / nchunk  # floating point division
        breakpoints <- as.integer(cumsum(rep.int(chunksize, nchunk)))
        ## The last value in 'breakpoints' *should* be 'totalsize' but there is
        ## always some uncertainty about what coercing the result of a floating
        ## point operation to integer will produce. So we set this value
        ## manually to 'totalsize' just in case.
        breakpoints[[nchunk]] <- totalsize
    } else {
        if (missing(chunksize))
            stop("one of 'nchunk' and 'chunksize' must be specified")
        ## All chunks will have the requested size, except maybe the last one.
        chunksize <- .normarg_nchunk_or_chunksize(chunksize, totalsize,
                                                  "chunksize")
        if (totalsize == 0L)
            return(PartitioningByEnd())
        quot <- totalsize %/% chunksize  # integer division
        breakpoints <- cumsum(rep.int(chunksize, quot))
        if (quot == 0L || breakpoints[[quot]] != totalsize)
            breakpoints <- c(breakpoints, totalsize)
    }
    PartitioningByEnd(breakpoints)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### equisplit()
###

### Will work out-of-the box on any object 'x' that supoorts elementNROWS()
### [, windows(), and relist() e.g. IRanges, GRanges, DNAStringSet,
### GAlignments objects and more...
equisplit <- function(x, nchunk, chunksize)
{
    x_eltNROWS <- elementNROWS(x)
    q <- breakInChunks(sum(x_eltNROWS), nchunk=nchunk, chunksize=chunksize)
    s <- PartitioningByWidth(x_eltNROWS)
    hits <- findOverlaps(q, s)
    unlisted_ans <- x[subjectHits(hits)]
    Ltrim <- pmax(start(q)[queryHits(hits)] - start(s)[subjectHits(hits)], 0L)
    Rtrim <- pmax(end(s)[subjectHits(hits)] - end(q)[queryHits(hits)], 0L)
    unlisted_ans <- windows(unlisted_ans, start=1L+Ltrim, end=-1L-Rtrim)
    relist(unlisted_ans, as(hits, "PartitioningByEnd"))
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "centeredIRanges" function.
###

centeredIRanges <- function(center, flank)
{
    if (!is.numeric(center))
        stop("'center' must be a numeric vector")
    if (!is.numeric(flank))
        stop("'flank' must be a numeric vector")
    IRanges(start=center-flank, end=center+flank)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "whichAsIRanges" function.
###
### Note that unlike the standard which() function, whichAsIRanges() drops
### the names of 'x'.
###

whichAsIRanges <- function(x)
{
    if (!is.logical(x))
        stop("'x' must be a logical vector")
    as(x, "NormalIRanges")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercing an IRanges object to a NormalIRanges object.
###

asNormalIRanges <- function(x, force=TRUE)
{
    if (!is(x, "IntegerRanges"))
        stop("'x' must be an IntegerRanges object")
    else if (!is(x, "IRanges"))
        x <- as(x, "IRanges")
    if (!isTRUEorFALSE(force))
        stop("'force' must be TRUE or FALSE")
    if (force)
        x <- reduce(x, drop.empty.ranges=TRUE)
    newNormalIRangesFromIRanges(x, check=!force)
}

.asNormalIRanges <- function(from) asNormalIRanges(from, force=TRUE)

setAs("IRanges", "NormalIRanges", .asNormalIRanges)

