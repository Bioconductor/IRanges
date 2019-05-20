### =========================================================================
### Intra-range methods
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### update_ranges()
###

### For internal use by the intra-range methods.
### Update the ranges of a Ranges derivative. Return an object of the same
### class as and parallel to 'x'.
setGeneric("update_ranges", signature="x",
    function(x, start=NULL, end=NULL, width=NULL, use.names=TRUE)
        standardGeneric("update_ranges")
)

### Shallow check: only check type and length, not the content.
.check_start_or_end_or_width <- function(start, x_len)
{
    if (is.null(start))
        return()
    stopifnot(is.integer(start))
    stopifnot(length(start) == x_len)
}

### Does not validate the modified object.
setMethod("update_ranges", "IRanges",
    function(x, start=NULL, end=NULL, width=NULL, use.names=TRUE)
    {
        use.names <- S4Vectors:::normargUseNames(use.names)
        narg <- sum(!is.null(start), !is.null(end), !is.null(width))
        if (narg == 0L) {
            if (!(use.names || is.null(x@NAMES)))
                x@NAMES <- NULL
            return(x)
        }
        stopifnot(narg <= 2L)
        x_len <- length(x)
        .check_start_or_end_or_width(start, x_len)
        .check_start_or_end_or_width(end, x_len)
        .check_start_or_end_or_width(width, x_len)
        if (narg == 2L) {
            if (!is.null(end)) {
                if (is.null(start)) {
                    ## 'end' and 'width' supplied
                    start <- end - width + 1L
                } else {
                    ## 'start' and 'end' supplied
                    width <- end - start + 1L
                }
            }
            args <- list(start=start, width=width)
        } else {
            stopifnot(is.null(width))
            if (is.null(start)) {
                ## only 'end' supplied
                width <- end - x@start + 1L
                args <- list(width=width)
            } else {
                ## only 'start' supplied
                width <- x@width - (start - x@start)
                args <- list(start=start, width=width)
            }
        }
        if (use.names) {
            more_args <- list(check=FALSE)
        } else {
            more_args <- list(NAMES=NULL, check=FALSE)
        }
        args <- c(list(x), args, more_args)
        do.call(BiocGenerics:::replaceSlots, args)
    }
)

setMethod("update_ranges", "Views",
    function(x, start=NULL, end=NULL, width=NULL, use.names=TRUE)
    {
        x@ranges <- update_ranges(x@ranges, start=start,
                                            end=end,
                                            width=width,
                                            use.names=use.names)
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### shift()
###

setGeneric("shift", signature="x",
    function(x, shift=0L, use.names=TRUE) standardGeneric("shift")
)

### Returns an NA-free integer vector of length 1 or 'x_len'.
### If the user-supplied 'shift' vector is constant then the returned vector
### is guaranteed to be of length <= 1.
.normarg_shift <- function(shift, x_len)
{
    if (!is.numeric(shift))
        stop("'shift' must be a numeric vector")
    if (!is.integer(shift))
        shift <- as.integer(shift)
    shift_len <- length(shift)
    if (shift_len == 0L) {
        if (x_len != 0L)
            stop(wmsg("'length(shift)' is 0 but 'length(x)' is not"))
        return(shift)
    }
    if (shift_len == 1L) {
        if (is.na(shift))
            stop(wmsg("'shift' cannot be NA"))
        return(shift)
    }
    if (shift_len > x_len)
        stop(wmsg("'length(shift)' is greater than 'length(x)'"))
    if (x_len %% length(shift) != 0L)
        warning(wmsg("'length(x)' is not a multiple of 'length(shift)'"))
    if (anyNA(shift))
        stop(wmsg("'shift' cannot contain NAs"))
    if (isConstant(shift))
        return(shift[[1L]])
    suppressWarnings(S4Vectors:::recycleVector(shift, x_len))
}

setMethod("shift", "Ranges",
    function(x, shift=0L, use.names=TRUE)
    {
        shift <- .normarg_shift(shift, length(x))
        if (is(x, "NormalIRanges") && length(shift) >= 2L)
            stop("'shift' must be a single number when shifting ",
                 "a NormalIRanges object")
        new_start <- start(x) + shift
        x <- update_ranges(x, start=new_start,
                              width=width(x),
                              use.names=use.names)
        validObject(x)
        x
    }
)

### Overwrite above method with optimized method for IPos objects.
### An IPos object cannot hold names so the 'use.names' arg has no effect.
### NOTE: We only support shifting by a single value at the moment!
setMethod("shift", "IPos",
    function(x, shift=0L, use.names=TRUE)
    {
        shift <- .normarg_shift(shift, length(x))
        if (is(x, "UnstitchedIPos")) {
            new_pos <- pos(x) + shift
            ans <- BiocGenerics:::replaceSlots(x, pos=new_pos, check=FALSE)
            return(ans)
        }
        if (length(shift) >= 2L)
            stop("'shift' must be a single number when shifting ",
                 "a StitchedIPos object")
        new_pos_runs <- callGeneric(x@pos_runs, shift=shift)
        BiocGenerics:::replaceSlots(x, pos_runs=new_pos_runs, check=FALSE)
    }
)

setMethod("shift", "RangesList",
    function(x, shift=0L, use.names=TRUE)
    {
        if (!is(shift, "List"))
            shift <- as(shift, "List")
        shift <- S4Vectors:::VH_recycle(shift, x, "shift", "x")

        if (is(x, "CompressedRangesList")) {
            unlisted_shift <- unlist(shift, use.names=FALSE)
            new_unlistData <- shift(x@unlistData, shift=unlisted_shift,
                                                  use.names=use.names)
            ans <- BiocGenerics:::replaceSlots(x, unlistData=new_unlistData,
                                                  check=FALSE)
            return(ans)
        }
        mendoapply(shift, x, shift,
                          MoreArgs=list(use.names=use.names))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### narrow()
###

### The default "narrow" method calls windows() so we only need to implement
### a "windows" method for IntegerRanges objects to make narrow() work on
### these objects.
setMethod("windows", "Ranges",
    function(x, start=NA, end=NA, width=NA)
    {
        ir <- make_IRanges_from_windows_args(x, start, end, width)
        if (length(x) == 0L)
            return(x)
        ans_start <- start(x) + start(ir) - 1L
        ans_width <- width(ir)
        update_ranges(x, start=ans_start, width=ans_width)
    }
)

setMethod("narrow", "MaskCollection",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
    {
        solved_SEW <- solveUserSEWForSingleSeq(width(x), start, end, width)
        solved_start <- start(solved_SEW)
        solved_end <- end(solved_SEW)
        solved_width <- width(solved_SEW)
        x@nir_list <- lapply(nir_list(x),
            function(nir) shift(restrict(nir, start=solved_start, end=solved_end),
                                1L - solved_start)
        )
        x@width <- solved_width
        if (!S4Vectors:::normargUseNames(use.names))
            names(x) <- NULL
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### resize()
###

setGeneric("resize", signature="x",
    function(x, width, fix="start", use.names=TRUE, ...)
        standardGeneric("resize")
)

setMethod("resize", "Ranges",
    function(x, width, fix="start", use.names=TRUE)
    {
        if (is(x, "NormalIRanges"))
            stop("resizing a NormalIRanges object is not supported")
        lx <- length(x)
        if (!is.numeric(width) || S4Vectors:::anyMissing(width))
            stop("'width' must be a numeric vector without NA's")
        if (!is.integer(width))
            width <- as.integer(width)
        if (S4Vectors:::anyMissingOrOutside(width, 0L))
            stop("'width' values must be non-negative")
        if (!(is.character(fix) ||
              (is(fix, "Rle") && is.character(runValue(fix)))) ||
            (length(fix) == 0L && length(x) > 0L) ||
            (length(setdiff(unique(fix),
                            c("start", "end", "center"))) > 0)) {
            stop("'fix' must be a character vector or character Rle ",
                 "with values in \"start\", \"end\", and \"center\"")
        }
        if (!is(fix, "Rle"))
            fix <- Rle(fix)
        if (length(fix) != lx)
            fix <- rep(fix, length.out = lx)
        ans_width <- S4Vectors:::recycleVector(width, lx)
        ans_start <- start(x)
        if (!identical(runValue(fix), "start")) {
            fixEnd <- as(fix == "end", "IRanges")
            if (length(fixEnd) > 0) {
                value <- extractROWS(ans_start, fixEnd) +
                         (extractROWS(width(x), fixEnd) -
                          extractROWS(ans_width, fixEnd))
                ans_start <- replaceROWS(ans_start, fixEnd, value)
            }
            fixCenter <- as(fix == "center", "IRanges")
            if (length(fixCenter) > 0) {
                value <- extractROWS(ans_start, fixCenter) +
                         (extractROWS(width(x), fixCenter) -
                          extractROWS(ans_width, fixCenter)) %/% 2L
                ans_start <- replaceROWS(ans_start, fixCenter, value)
            }
        }
        update_ranges(x, start=ans_start, width=ans_width, use.names=use.names)
    }
)

setMethod("resize", "RangesList",
    function(x, width, fix="start", use.names=TRUE, ...)
    {
        if (!is(width, "List"))
            width <- as(width, "List")
        width <- S4Vectors:::VH_recycle(width, x, "width", "x")
        if (!is(fix, "List"))
            fix <- as(fix, "List")
        fix <- S4Vectors:::VH_recycle(fix, x, "fix", "x")

        if (is(x, "CompressedRangesList")) {
            unlisted_width <- unlist(width, use.names=FALSE)
            unlisted_fix <- unlist(fix, use.names=FALSE)
            new_unlistData <- resize(x@unlistData, width=unlisted_width,
                                                   fix=unlisted_fix,
                                                   use.names=use.names,
                                                   ...)
            ans <- BiocGenerics:::replaceSlots(x, unlistData=new_unlistData,
                                                  check=FALSE)
            return(ans)
        }
        mendoapply(resize, x, width, fix,
                           MoreArgs=list(use.names=use.names, ...))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### flank()
###

setGeneric("flank", signature="x",
    function(x, width, start=TRUE, both=FALSE, use.names=TRUE, ...)
        standardGeneric("flank")
)

setMethod("flank", "Ranges",
    function(x, width, start=TRUE, both=FALSE, use.names=TRUE)
    {
        if (is(x, "NormalIRanges"))
            stop("flanking a NormalIRanges object is not supported")
        width <- recycleIntegerArg(width, "width", length(x))
        if (!is.logical(start) || S4Vectors:::anyMissing(start))
            stop("'start' must be logical without NA's")
        start <- S4Vectors:::recycleVector(unname(start), length(x))
        if (!isTRUEorFALSE(both))
            stop("'both' must be TRUE or FALSE")
        ans_start <- integer(length(x))
        if (both) {
            idx1 <- which(start)
            idx2 <- which(!start)
            width <- abs(width)
            ans_width <- 2L * width
            ans_start[idx1] <- start(x)[idx1] - width[idx1]
            ans_start[idx2] <- end(x)[idx2] - width[idx2] + 1L
        } else {
            idx1a <- which(start & width >= 0L)
            idx1b <- which(start & width < 0L)
            idx2a <- which(!start & width >= 0L)
            idx2b <- which(!start & width < 0L)
            ans_width <- abs(width)
            ans_start[idx1a] <- start(x)[idx1a] - width[idx1a]
            ans_start[idx1b] <- start(x)[idx1b]
            ans_start[idx2a] <- end(x)[idx2a] + 1L
            ans_start[idx2b] <- end(x)[idx2b] + width[idx2b] + 1L
        }
        update_ranges(x, start=ans_start, width=ans_width, use.names=use.names)
    }
)

setMethod("flank", "RangesList",
    function(x, width, start=TRUE, both=FALSE, use.names=TRUE, ...)
    {
        if (!is(width, "List"))
            width <- as(width, "List")
        width <- S4Vectors:::VH_recycle(width, x, "width", "x")
        if (!is(start, "List"))
            start <- as(start, "List")
        start <- S4Vectors:::VH_recycle(start, x, "start", "x")

        if (is(x, "CompressedRangesList")) {
            unlisted_width <- unlist(width, use.names=FALSE)
            unlisted_start <- unlist(start, use.names=FALSE)
            new_unlistData <- flank(x@unlistData, width=unlisted_width,
                                                  start=unlisted_start,
                                                  both=both,
                                                  use.names=use.names,
                                                  ...)
            ans <- BiocGenerics:::replaceSlots(x, unlistData=new_unlistData,
                                                  check=FALSE)
            return(ans)
        }
        mendoapply(flank, x, width, start,
                          MoreArgs=list(both=both, use.names=use.names, ...))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### promoters()
###

setGeneric("promoters", signature="x",
    function(x, upstream=2000, downstream=200, use.names=TRUE, ...)
        standardGeneric("promoters")
)

setMethod("promoters", "IntegerRanges",
    function(x, upstream=2000, downstream=200, use.names=TRUE)
    {
        if (is(x, "NormalIRanges"))
            stop("promoters() is not supported on a NormalIRanges object")
        x_len <- length(x)
        upstream <- recycleIntegerArg(upstream, "upstream", x_len)
        downstream <- recycleIntegerArg(downstream, "downstream", x_len)
        if (x_len == 0L)
            return(update_ranges(x, use.names=use.names))
        if (min(upstream) < 0L || min(downstream) < 0L)
            stop("'upstream' and 'downstream' must be integers >= 0")
        x_start <- start(x)
        new_start <- x_start - upstream
        new_end <- x_start + downstream - 1L
        update_ranges(x, start=new_start, end=new_end, use.names=use.names)
    }
)

setMethod("promoters", "RangesList",
    function(x, upstream=2000, downstream=200, use.names=TRUE)
    {
        if (!is(upstream, "List"))
            upstream <- as(upstream, "List")
        upstream <- S4Vectors:::VH_recycle(upstream, x, "upstream", "x")
        if (!is(downstream, "List"))
            downstream <- as(downstream, "List")
        downstream <- S4Vectors:::VH_recycle(downstream, x, "downstream", "x")

        if (is(x, "CompressedRangesList")) {
            unlisted_upstream <- unlist(upstream, use.names=FALSE)
            unlisted_downstream <- unlist(downstream, use.names=FALSE)
            new_unlistData <- promoters(x@unlistData,
                                        upstream=unlisted_upstream,
                                        downstream=unlisted_downstream,
                                        use.names=use.names)
            ans <- BiocGenerics:::replaceSlots(x, unlistData=new_unlistData,
                                                  check=FALSE)
            return(ans)
        }
        mendoapply(promoters, x, upstream, downstream,
                              MoreArgs=list(use.names=use.names))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### reflect()
###

setGeneric("reflect", signature="x",
    function(x, bounds, use.names=TRUE)
        standardGeneric("reflect")
)

setMethod("reflect", "IntegerRanges",
    function(x, bounds, use.names=TRUE)
    {
        if (is(x, "NormalIRanges"))
            stop("reflecting a NormalIRanges object is not supported")
        if (!is(bounds, "IntegerRanges"))
            stop("'bounds' must be an IntegerRanges object")
        if (length(x) > 1 && length(bounds) == 0)
            stop("'bounds' is an empty IntegerRanges object")
        if (length(x) < length(bounds))
            bounds <- head(bounds, length(x))
        ans_start <- (2L * start(bounds) + width(bounds) - 1L) - end(x)
        update_ranges(x, start=ans_start, width=width(x), use.names=use.names)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### restrict()
###

setGeneric("restrict", signature="x",
    function(x, start=NA, end=NA, keep.all.ranges=FALSE, use.names=TRUE)
        standardGeneric("restrict")
)

### We distinguish 4 exclusive types of relationship between a range in 'x'
### and its corresponding restriction interval (specified by 'start' and
### 'end'):
###   - Type 1: They have a non-empty intersection.
###   - Type 2: The restriction interval is empty but its bounds are within
###             the range in 'x'.
###   - Type 3: The restriction interval is not empty and is adjacent to the
###             range in 'x' i.e. the range in 'x' ends at start - 1 or starts
###             at end - 1.
###
### drop.ranges.mode:
###   0L: Ranges in 'x' that are empty after restriction are dropped.
###   1L: Ranges in 'x' that are not overlapping and not even adjacent
###       with the region of restriction are dropped.
###       "Not overlapping and not adjacent" means that they end strictly
###       before start - 1 or start strictly after end + 1.
###       Those that are not overlapping but are however adjacent are kept
###       (and are empty after restriction).
###   2L: All ranges in 'x' are kept after restriction.
### Note that the only mode compatible with a NormalIRanges object is 0L.
.restrict_IntegerRanges <- function(x, start, end, drop.ranges.mode, use.names)
{
    if (!S4Vectors:::isNumericOrNAs(start))
        stop("'start' must be a vector of integers")
    if (!is.integer(start))
        start <- as.integer(start)
    if (!S4Vectors:::isNumericOrNAs(end))
        stop("'end' must be a vector of integers")
    if (!is.integer(end))
        end <- as.integer(end)
    if (length(x) != 0L) {
        if (length(start) == 0L || length(start) > length(x))
            stop("invalid 'start' length")
        if (length(end) == 0L || length(end) > length(x))
            stop("invalid 'end' length")
    }
    start <- S4Vectors:::recycleVector(start, length(x))
    end <- S4Vectors:::recycleVector(end, length(x))
    use.names <- S4Vectors:::normargUseNames(use.names)

    ans_start <- start(x)
    ans_end <- end(x)
    if (use.names) ans_names <- names(x) else ans_names <- NULL
    ans_mcols <- mcols(x, use.names=FALSE)

    ## Compare ranges in 'x' with 'start'.
    if (drop.ranges.mode == 0L)
        far_too_left <- !is.na(start) & (ans_end < start)
    else
        far_too_left <- !is.na(start) & (ans_end < start - 1L)
    if (drop.ranges.mode == 2L) {
        ans_end[far_too_left] <- start[far_too_left] - 1L
    } else {
        ## Drop the ranges that are far too left with respect to the
        ## region of restriction.
        keep_idx <- which(!far_too_left)
        ans_start <- ans_start[keep_idx]
        ans_end <- ans_end[keep_idx]
        if (!is.null(ans_names))
            ans_names <- ans_names[keep_idx]
        ans_mcols <- extractROWS(ans_mcols, keep_idx)
        start <- start[keep_idx]
        end <- end[keep_idx]
    }
    ## Fix 'ans_start'.
    too_left <- !is.na(start) & (ans_start < start)
    ans_start[too_left] <- start[too_left]

    ## Compare ranges in 'x' with 'end'.
    if (drop.ranges.mode == 0L)
        far_too_right <- !is.na(end) & (ans_start > end)
    else
        far_too_right <- !is.na(end) & (ans_start > end + 1L)
    if (drop.ranges.mode == 2L) {
        ans_start[far_too_right] <- end[far_too_right] + 1L
    } else {
        ## Drop the ranges that are far too right with respect to the
        ## region of restriction.
        keep_idx <- which(!far_too_right)
        ans_start <- ans_start[keep_idx]
        ans_end <- ans_end[keep_idx]
        if (!is.null(ans_names))
            ans_names <- ans_names[keep_idx]
        ans_mcols <- extractROWS(ans_mcols, keep_idx)
        start <- start[keep_idx]
        end <- end[keep_idx]
    }
    ## Fix 'ans_end'.
    too_right <- !is.na(end) & (ans_end > end)
    ans_end[too_right] <- end[too_right]

    ans_width <- ans_end - ans_start + 1L
    BiocGenerics:::replaceSlots(x, start=ans_start,
                                   width=ans_width,
                                   NAMES=ans_names,
                                   mcols=ans_mcols,
                                   check=FALSE)
}

setMethod("restrict", "IntegerRanges",
    function(x, start=NA, end=NA, keep.all.ranges=FALSE, use.names=TRUE)
    {
        if (!isTRUEorFALSE(keep.all.ranges))
            stop("'keep.all.ranges' must be TRUE or FALSE")
        use.names <- S4Vectors:::normargUseNames(use.names)
        if (is(x, "NormalIRanges")) {
            if (keep.all.ranges)
                stop("'keep.all.ranges=TRUE' is not supported ",
                     "when 'x' is a NormalIRanges object")
            drop.ranges.mode <- 0L
        } else {
            if (keep.all.ranges)
                drop.ranges.mode <- 2L
            else
                drop.ranges.mode <- 1L
        }
        .restrict_IntegerRanges(x, start, end, drop.ranges.mode, use.names)
    }
)

setMethod("restrict", "Views",
    function(x, start=NA, end=NA, keep.all.ranges=FALSE, use.names=TRUE)
    {
        new_ranges <- restrict(ranges(x), start=start, end=end,
                                          keep.all.ranges=keep.all.ranges,
                                          use.names=use.names)
        BiocGenerics:::replaceSlots(x, ranges=new_ranges, check=FALSE)
    }
)

setMethod("restrict", "RangesList",
    function(x, start=NA, end=NA, keep.all.ranges=FALSE, use.names=TRUE)
    {
        if (!isTRUEorFALSE(keep.all.ranges))
            stop("'keep.all.ranges' must be TRUE or FALSE")
        if (!is(start, "List"))
            start <- as(start, "List")
        start <- S4Vectors:::VH_recycle(start, x, "start", "x")
        if (!is(end, "List"))
            end <- as(end, "List")
        end <- S4Vectors:::VH_recycle(end, x, "end", "x")

        if (is(x, "CompressedRangesList") && keep.all.ranges) {
            unlisted_start <- unlist(start, use.names=FALSE)
            unlisted_end <- unlist(end, use.names=FALSE)
            new_unlistData <- restrict(x@unlistData, start=unlisted_start,
                                                     end=unlisted_end,
                                                     keep.all.ranges=TRUE,
                                                     use.names=use.names)
            ans <- BiocGenerics:::replaceSlots(x, unlistData=new_unlistData,
                                                  check=FALSE)
            return(ans)
        }
        mendoapply(restrict, x, start, end,
                             MoreArgs=list(keep.all.ranges=keep.all.ranges,
                                           use.names=use.names))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### threebands()
###

setGeneric("threebands", signature="x",
    function(x, start=NA, end=NA, width=NA)
        standardGeneric("threebands")
)

### Method for IRanges only!
setMethod("threebands", "IRanges",
    function(x, start=NA, end=NA, width=NA)
    {
        middle <- narrow(x, start=start, end=end, width=width, use.names=FALSE)
        left <- right <- middle
        left@start <- start(x)
        left@width <- start(middle) - start(x)
        right@start <- end(middle) + 1L
        right@width <- end(x) - end(middle)
        list(left=left, middle=middle, right=right)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Zooming (symmetrically scales the width).
###
### TODO: Implement a zoom() generic and methods and make the "Ops" method
### below a simple convenience wrapper for zoom(). Recommend the use of zoom()
### over "Ops" methods in packages and scripts. Reserve "Ops" methods as a
### convenience when working interactively.
###

setMethod("Ops", c("Ranges", "numeric"),
    function(e1, e2)
    {
        if (S4Vectors:::anyMissing(e2))
            stop("NA not allowed as zoom factor")
        if ((length(e1) < length(e2) && length(e1)) ||
            (length(e1) && !length(e2)) ||
            (length(e1) %% length(e2) != 0))
            stop("zoom factor length not a multiple of number of ranges")
        if (.Generic == "*") {
            e2 <- ifelse(e2 < 0, abs(1/e2), e2)
            r <- e1
            mid <- (start(r)+end(r))/2
            w <- width(r)/e2
            update_ranges(r, start = as.integer(ceiling(mid - w/2)), width = as.integer(floor(w)))
        } else {
            if (.Generic == "-") {
                e2 <- -e2
                .Generic <- "+"
            }
            if (.Generic == "+") {
                if (any(-e2*2 > width(e1)))
                    stop("adjustment would result in ranges with negative widths")
                update_ranges(e1, start = as.integer(start(e1) - e2), end = as.integer(end(e1) + e2))
            }
        }
    }
)

setMethod("Ops", c("RangesList", "numeric"),
          function(e1, e2)
          {
            for (i in seq_len(length(e1)))
              e1[[i]] <- callGeneric(e1[[i]], e2)
            e1
          })

setMethod("Ops", c("CompressedRangesList", "numeric"),
          function(e1, e2)
          {
            relist(callGeneric(unlist(e1, use.names = FALSE), e2), e1)
          })

