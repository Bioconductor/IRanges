### =========================================================================
### Set operations on IRanges objects
### -------------------------------------------------------------------------
###
### I. Vector-wise set operations: union, intersect, setdiff
###
### All the functions in that group are implemented to behave like
### endomorphisms with respect to their first argument 'x'.
### If 'x' is an IRanges object then the returned IRanges object is also
### guaranteed to be normal (note that if 'x' is an IRanges *instance* then
### the returned object is still an IRanges instance, so it is *not* promoted
### to NormalIRanges).
### Finally, the functions in that group interpret each supplied IRanges
### object ('x' or 'y') as a set of integer values. Therefore, if 2 IRanges
### objects 'x1' and 'x2' represent the same set of integers, then each of
### these functions will return the same result when 'x1' is replaced by 'x2'
### in the input.
###
### II. Element-wise (aka "parallel") set operations: punion, pintersect,
###     psetdiff, pgap
###
### The functions in that group take 2 *objects* of the same length and
### return an object of the same class and length as the first argument.
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### union()
###

setMethod("union", c("IRanges", "IRanges"),
    function(x, y, ...)
    {
        ## We need to downgrade 'x' to an IRanges instance 'x0' so 'c(x0, y)'
        ## is guaranteed to work (even e.g. if 'x' is a NormalIRanges object).
        x0 <- as(x, "IRanges")  # downgrade x to IRanges
        x0 <- reduce(c(x0, y), drop.empty.ranges=TRUE)
        ## Maybe the call to update() below could be replaced by
        ## 'as(x, "IRanges") <- x0' but I was not lucky with my first
        ## attempt to use this construct:
        ##   > v <- Views(XInteger(18), 2:5, 13:10)
        ##   > as(v, "IRanges") <- IRanges(3, 8)
        ##   Error: evaluation nested too deeply: infinite recursion / options(expressions=)?
        initialize(x, start=start(x0), width=width(x0), NAMES=names(x0),
                   elementMetadata=NULL)
    }
)

setMethod("union", c("RangesList", "RangesList"),
          function(x, y) mendoapply(union, x, y))

setMethod("union", c("CompressedIRangesList", "CompressedIRangesList"),
          function(x, y) {
            len <- max(length(x), length(y))
            if (length(x) != len)
              x <- x[recycleVector(seq_len(length(x)), len)]
            if (length(y) != len)
              y <- y[recycleVector(seq_len(length(y)), len)]
            xy <- c(unlist(x, use.names = FALSE), unlist(y, use.names = FALSE))
            xy_list <- split(xy, factor(c(togroup(x), togroup(y)),
                                        seq_len(length(x))))
            names(xy_list) <- names(x)
            reduce(xy_list, drop.empty.ranges=TRUE)
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### intersect()
###

setMethod("intersect", c("IRanges", "IRanges"),
    function(x, y, ...)
    {
        if (length(x) == 0L)
            return(x)
        start <- min(c(start(x), start(y)))
        end <- max(c(end(x), end(y)))
        setdiff(x, gaps(y, start=start, end=end))
    }
)

setMethod("intersect", c("RangesList", "RangesList"),
          function(x, y) mendoapply(intersect, x, y))

setMethod("intersect", c("CompressedIRangesList", "CompressedIRangesList"),
          function(x, y) {
            nonempty <- elementLengths(x) != 0L
            rx <- unlist(range(x), use.names = FALSE)
            startx <- integer()
            startx[nonempty] <- start(rx)
            endx <- integer()
            endx[nonempty] <- end(rx)
            setdiff(x, gaps(y, start = startx, end = endx))
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### setdiff()
###

setMethod("setdiff", c("IRanges", "IRanges"),
    function(x, y, ...)
    {
        if (length(x) == 0L)
            return(x)
        start <- min(c(start(x), start(y)))
        end <- max(c(end(x), end(y)))
        gaps(union(gaps(x, start=start, end=end), y), start=start, end=end)
    }
)

setMethod("setdiff", c("RangesList", "RangesList"),
          function(x, y) mendoapply(setdiff, x, y))

setMethod("setdiff", c("CompressedIRangesList", "CompressedIRangesList"),
          function(x, y) {
            nonempty <- elementLengths(x) != 0L
            rx <- unlist(range(x), use.names = FALSE)
            startx <- integer()
            startx[nonempty] <- start(rx)
            endx <- integer()
            endx[nonempty] <- end(rx)
            gaps(union(gaps(x), y), start = startx, end = endx)
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### punion()
###

setGeneric("punion", signature=c("x", "y"),
    function(x, y, ...) standardGeneric("punion")
)

setMethod("punion", c("IRanges", "IRanges"),
    function(x, y, fill.gap=FALSE, ...)
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
        ans_names <- names(x)
        if (is.null(ans_names))
            ans_names <- names(y)
        IRanges(start=ans_start, end=ans_end, names=ans_names)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### pintersect()
###

setGeneric("pintersect", signature=c("x", "y"),
    function(x, y, ...) standardGeneric("pintersect")
)

setMethod("pintersect", c("IRanges", "IRanges"),
    function(x, y, resolve.empty=c("none", "max.start", "start.x"), ...)
    {
        if (length(x) != length(y))
            stop("'x' and 'y' must have the same length")

        ans_start <- pmax.int(start(x), start(y))
        ans_end <- pmin.int(end(x), end(y))
        ans_width <- ans_end - ans_start + 1L

        keep_empty_x <- width(x) == 0L
        if (any(keep_empty_x)) {
            keep_empty_x <- keep_empty_x &
              ((start(x) >= start(y) & start(x) <= end(y)) | 
               (start(x) == start(y) & width(y) == 0L))
        }
        if (any(keep_empty_x)) {
            ans_start[keep_empty_x] <- start(x)[keep_empty_x]
            ans_width[keep_empty_x] <- 0L
        }

        keep_empty_y <- width(y) == 0L
        if (any(keep_empty_y)) {
            keep_empty_y <- keep_empty_y &
              start(y) >= start(x) & start(y) <= end(x)
        }
        if (any(keep_empty_y)) {
            ans_start[keep_empty_y] <- start(y)[keep_empty_y]
            ans_width[keep_empty_y] <- 0L
        }

        check_empty <- ans_width < 0L
        check_empty[keep_empty_x | keep_empty_y] <- FALSE
        if (any(check_empty)) {
            resolve.empty <- match.arg(resolve.empty)
            if (resolve.empty == "none") {
                stop("some intersections produce ambiguous empty ranges.\n",
                     "  Use argument 'resolve.empty' to resolve them.")
            } else {
                ans_width[check_empty] <- 0L
                if (resolve.empty == "start.x")
                    ans_start[check_empty] <- start(x)[check_empty]
            }
        }
        ans_names <- names(x)
        if (is.null(ans_names))
            ans_names <- names(y)
        IRanges(start=ans_start, width=ans_width, names=ans_names)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### psetdiff()
###

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
        ans_names <- names(x)
        if (is.null(ans_names))
            ans_names <- names(y)
        IRanges(start=ans_start, end=ans_end, names=ans_names)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### pgap()
###

setGeneric("pgap", signature=c("x", "y"),
    function(x, y, ...) standardGeneric("pgap")
)

setMethod("pgap", c("IRanges", "IRanges"),
    function(x, y, ...)
    {
        if (length(x) != length(y))
            stop("'x' and 'y' must have the same length")
        ans_end_plus1 <- pmax.int(start(x), start(y))
        ans_start <- pmin.int(end(x), end(y)) + 1L
        ans_width <- ans_end_plus1 - ans_start
        ans_width[ans_width < 0L] <- 0L
        ans_names <- names(x)
        if (is.null(ans_names))
            ans_names <- names(y)
        IRanges(start=ans_start, width=ans_width, names=ans_names)
    }
)

