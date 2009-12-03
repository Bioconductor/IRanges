### =========================================================================
### Set operations on IRanges objects
### -------------------------------------------------------------------------
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Vector-wise operations.
###
### All the functions in that section are implemented to behave like
### endomorphisms with respect to their first argument 'x' (an IRanges
### object).
### The returned IRanges object is guaranteed to be normal.
### Finally, each of these function interprets each supplied IRanges object
### ('x' or 'y') as a set of integer values. Therefore, if 2 IRanges objects
### 'x1' and 'x2' represent the same set of integers, then each of these
### functions will return the same result when 'x1' is replaced by 'x2' in
### the input.
###

setMethod("gaps", "IRanges",
    function(x, start=NA, end=NA)
    {
        start <- normargSingleStartOrNA(start)
        end <- normargSingleEndOrNA(end)
        ## No matter in what order restricting and normalizing are done, the final
        ## result should always be exactly the same.
        ## Now which order is the most efficient? It depends...
        xx <- asNormalIRanges(x, force=TRUE)
        xx0 <- restrict(xx, start=start, end=end) # preserves normality
        ans_start <- ans_width <- integer(0)
        if (!isEmpty(xx0)) {
            start0 <- start(xx0)
            end0 <- end(xx0)
            if (!is.na(start) && start < min(xx0)) {
                start0 <- c(start, start0)
                end0 <- c(start - 1L, end0)
            }
            if (!is.na(end) && max(xx0) < end) {
                start0 <- c(start0, end + 1L)
                end0 <- c(end0, end)
            }
            if (length(start0) >= 2) {
                ans_start <- end0[-length(start0)] + 1L
                ans_width <- start0[-1L] - ans_start
            }
        } else if (!is.na(start) || !is.na(end)) {
            if (is.na(start) || is.na(end))
                stop("'x' is not overlapping with the unbounded region ",
                     "represented by 'start' and 'end'")
            ans_start <- start
            ans_width <- end - start + 1L
        }
        unsafe.update(x, start=ans_start, width=ans_width, names=NULL)
    }
)

setMethod("union", c("IRanges", "IRanges"),
    function(x, y)
    {
        ## We need to downgrade 'x' to an IRanges instance 'x0' so 'c(x0, y)'
        ## is guaranteed to work (even e.g. if 'x' is a NormalIRanges object).
        x0 <- as(x, "IRanges")  # downgrade x to IRanges
        x0 <- reduce(c(x0, y))
        x0 <- x0[width(x0) != 0]
        ## Maybe the call to update() below could be replaced by
        ## 'as(x, "IRanges") <- x0' but I was not lucky with my first
        ## attempt to use this construct:
        ##   > v <- Views(XInteger(18), 2:5, 13:10)
        ##   > as(v, "IRanges") <- IRanges(3, 8)
        ##   Error: evaluation nested too deeply: infinite recursion / options(expressions=)?
        update(x, start=start(x0), width=width(x0), names=names(x0))
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Element-wise (aka "parallel") operations.
###
### The functions below take 2 IRanges *objects* and return an IRanges
### *instance*. Hence they are NOT endomorphisms.
###

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

setGeneric("pgap", signature=c("x", "y"),
    function(x, y, ...) standardGeneric("pgap")
)

setMethod("pgap", c("IRanges", "IRanges"),
    function(x, y, ...)
    {
        if (length(x) != length(y))
            stop("'x' and 'y' must have the same length")
        ans_start <- pmax.int(start(x), start(y))
        ans_end <- pmin.int(end(x), end(y)) + 1L
        ans_width <- ans_start - ans_end
        ans_width[ans_width < 0L] <- 0L
        IRanges(start=ans_end, width=ans_width)
    }
)

