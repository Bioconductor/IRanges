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
    if (anyMissingOrOutside(width, 0L))
        stop("'width' cannot contain NAs or negative values")
    if (!is.numeric(gapwidth))
        stop("'gapwidth' must be an integer vector")
    if (!is.integer(gapwidth))
        gapwidth <- as.integer(gapwidth)
    if (anyMissing(gapwidth))
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
### The "breakInChunks" function.
###

breakInChunks <- function(totalsize, chunksize)
{
    quot <- totalsize %/% chunksize
    ans_width <- rep.int(chunksize, quot)
    rem <- totalsize %% chunksize
    if (rem > 0L)
        ans_width <- c(ans_width, rem)
    PartitioningByWidth(ans_width)
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

