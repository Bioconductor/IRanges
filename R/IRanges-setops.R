### =========================================================================
### Set operations on IRanges objects
### -------------------------------------------------------------------------
###
### All these functions take 2 IRanges *objects* and return an IRanges
### *instance*. Hence they are NOT endomorphisms.
###

setMethod("union", c("IRanges", "IRanges"),
    function(x, y)
    {
        z <- reduce(c(x, y))
        z[width(z) != 0]
    }
)

setMethod("intersect", c("IRanges", "IRanges"),
    function(x, y)
    {
        start <- min(c(start(x), start(y)))
        end <- max(c(end(x), end(y)))
        setdiff(x, gaps(y, start=start, end=end))
    }
)

setMethod("setdiff", c("IRanges", "IRanges"),
    function(x, y)
    {
        start <- min(c(start(x), start(y)))
        end <- max(c(end(x), end(y)))
        gaps(union(gaps(x, start=start, end=end), y), start=start, end=end)
    }
)

setGeneric("punion", signature=c("x", "y"),
    function(x, y, ...) standardGeneric("punion")
)

setMethod("punion", c("IRanges", "IRanges"),
    function(x, y, fill.gap=FALSE)
    {
        if (length(x) != length(y))
            stop("'x' and 'y' must have the same length")
        if (!isTRUEorFALSE(fill.gap))
            stop("'fill.gap' must be TRUE or FALSE")
        if (!fill.gap) {
            gap <- pmax.int(start(x), start(y)) -
                   pmin.int(end(x), end(y)) - 1L
            if (any(gap > 0L))
                stop("some pair of ranges have a gap within ",
                     "the 2 members of the pair.\n",
                     "  Use 'fill.gap=TRUE' to enforce their ",
                     "union by filling the gap.")
        }
        ans_start <- pmin.int(start(x), start(y))
        ans_end <- pmax.int(end(x), end(y))
        IRanges(start=ans_start, end=ans_end)
    }
)

setGeneric("pintersect", signature=c("x", "y"),
    function(x, y, ...) standardGeneric("pintersect")
)

setMethod("pintersect", c("IRanges", "IRanges"),
    function(x, y, ...)
    {
        if (length(x) != length(y))
            stop("'x' and 'y' must have the same length")
        ans_start <- pmax.int(start(x), start(y))
        ans_end <- pmin.int(end(x), end(y))
        ans_width <- ans_end - ans_start + 1L
        ans_width[ans_width < 0L] <- 0L
        IRanges(start=ans_start, width=ans_width)
    }
)

setGeneric("psetdiff", signature=c("x", "y"),
    function(x, y, ...) standardGeneric("psetdiff")
)

setMethod("psetdiff", c("IRanges", "IRanges"),
    function(x, y, ...)
    {
        if (length(x) != length(y))
            stop("'x' and 'y' must have the same length")
        ans_start <- start(x)
        ans_end <- end(x)
        if (any((start(y) > ans_start) & (end(y) < ans_end)))
            stop("some ranges in 'y' have their end points strictly inside\n",
                 "  the range in 'x' that they need to be subtracted from.\n",
                 "  Cannot subtract them.")
        start2 <- pmax.int(ans_start, start(y))
        end2 <- pmin.int(ans_end, end(y))
        ii <- start2 <= end2
        jj <- end2 == ans_end
        kk <- ii & jj
        ans_end[kk] <- start2[kk] - 1L
        kk <- ii & (!jj)
        ans_start[kk] <- end2[kk] + 1L
        IRanges(start=ans_start, end=ans_end)
    }
)

