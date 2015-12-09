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

.normarg_chunksize_or_nchunk <- function(chunksize, totalsize, what)
{
    if (!isSingleNumber(chunksize))
        stop("'", what, "' must be a single integer")
    if (!is.integer(chunksize))
        chunksize <- as.integer(chunksize)
    if (chunksize < 0L)
        stop("'", what, "' cannot be negative")
    if (chunksize == 0L && totalsize != 0L)
        stop("'", what, "' can be 0 only if 'totalsize' is 0")
    chunksize
}

### TODO: Argument names and order is inconsistent with tileGenome().
### Reconcile them!
breakInChunks <- function(totalsize, chunksize, nchunk)
{
    totalsize <- .normarg_totalsize(totalsize)
    if (!missing(chunksize)) {
        if (!missing(nchunk)) 
            stop("only one of 'chunksize' and 'nchunk' can be specified")
        ## All chunks will have the requested size, except maybe the last one.
        chunksize <- .normarg_chunksize_or_nchunk(chunksize, totalsize,
                                                  "chunksize")
        if (totalsize == 0L)
            return(PartitioningByEnd())
        quot <- totalsize %/% chunksize  # integer division
        ans_end <- cumsum(rep.int(chunksize, quot))
        if (quot == 0L || ans_end[[quot]] != totalsize)
            ans_end <- c(ans_end, totalsize)
    } else {
        if (missing(nchunk))   
            stop("one of 'chunksize' and 'nchunk' must be specified")
        ## All chunks will have more or less the same size, with the difference
        ## between smallest and biggest chunks guaranteed to be <= 1.
        nchunk <- .normarg_chunksize_or_nchunk(nchunk, totalsize,
                                               "nchunk")
        if (nchunk == 0L)
            return(PartitioningByEnd())
        chunksize <- totalsize / nchunk  # floating point division
        ans_end <- as.integer(cumsum(rep.int(chunksize, nchunk)))
        ## The last value in 'ans_end' *should* be 'totalsize' but there is
        ## always some uncertainty about what coercing the result of a floating
        ## point operation to integer will produce. So we set this value
        ## manually to 'totalsize' just in case.
        ans_end[[nchunk]] <- totalsize
    }
    PartitioningByEnd(ans_end)
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
    if (!is(x, "Ranges"))
        stop("'x' must be an Ranges object")
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

