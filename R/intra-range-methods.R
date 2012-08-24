### =========================================================================
### Intra-range methods
### -------------------------------------------------------------------------
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### shift()
###

setGeneric("shift", signature="x",
    function(x, shift=0L, use.names=TRUE) standardGeneric("shift")
)

setMethod("shift", "Ranges",
    function(x, shift=0L, use.names=TRUE)
    {
        if (is(x, "NormalIRanges") && !isSingleNumber(shift))
            stop("'shift' must be a single number when shifting ",
                 "a NormalIRanges object")
        shift <- recycleIntegerArg(shift, "shift", length(x))
        ## Note that we won't do anything with 'new_end', we just want to
        ## fail in case of integer overflow.
        errorIfWarning(new_start <- start(x) + shift)
        errorIfWarning(new_end <- new_start + width(x) - 1L)
        if (is(x, "IRanges")) {
            x@start <- new_start
        } else {
            x <- update(x, start=new_start, width=width(x), check=FALSE)
        }
        if (!normargUseNames(use.names))
            names(x) <- NULL
        x
    }
)

setMethod("shift", "Views",
    function(x, shift=0L, use.names=TRUE)
    {
        x@ranges <- shift(ranges(x), shift=shift, use.names=use.names)
        x
    }
)

setMethod("shift", "RangesList",
          function(x, shift=0L, use.names = TRUE)
          {
              lx <- length(x)
              shift <- normargAtomicList1(shift, IntegerList, lx)
              mendoapply("shift", x = x, shift = shift,
                         MoreArgs = list(use.names = use.names))
          })

setMethod("shift", "CompressedIRangesList",
          function(x, shift=0L, use.names = TRUE)
          {
              lx <- length(x)
              eln <- elementLengths(x)
              shift <- normargAtomicList2(shift, IntegerList, lx, eln)
              slot(x, "unlistData", check=FALSE) <-
                shift(x@unlistData, shift = shift, use.names = use.names)
              x
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### narrow()
###

setGeneric("narrow", signature="x",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
        standardGeneric("narrow")
)

setMethod("narrow", "Ranges",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
    {
        if (is(x, "NormalIRanges"))
            stop("narrowing a NormalIRanges object is not supported")
        solved_SEW <- solveUserSEW(width(x), start=start, end=end, width=width)
        ans_start <- start(x) + start(solved_SEW) - 1L
        ans_width <- width(solved_SEW)
        x <- update(x, start=ans_start, width=ans_width, check=FALSE)
        if (!normargUseNames(use.names))
            names(x) <- NULL
        x
    }
)

setMethod("narrow", "Views",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
    {
        x@ranges <- narrow(ranges(x), start=start, end=end, width=width,
                           use.names=use.names)
        x
    }
)

setMethod("narrow", "RangesList",
          function(x, start = NA, end = NA, width = NA, use.names = TRUE)
          {
              lx <- length(x)
              start <- normargAtomicList1(start, IntegerList, lx)
              end <- normargAtomicList1(end, IntegerList, lx)
              width <- normargAtomicList1(width, IntegerList, lx)
              mendoapply(narrow, x = x, start = start, end = end, width = width,
                         MoreArgs = list(use.names = use.names))
          })

setMethod("narrow", "CompressedIRangesList",
          function(x, start = NA, end = NA, width = NA, use.names = TRUE)
          {
              lx <- length(x)
              eln <- elementLengths(x)
              start <- normargAtomicList2(start, IntegerList, lx, eln)
              end <- normargAtomicList2(end, IntegerList, lx, eln)
              width <- normargAtomicList2(width, IntegerList, lx, eln)
              slot(x, "unlistData", check=FALSE) <-
                narrow(x@unlistData, start = start, end = end, width = width,
                       use.names = use.names)
              x
          })

setMethod("narrow", "MaskCollection",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
    {
        solved_SEW <- solveSubseqSEW(width(x), start, end, width)
        solved_start <- start(solved_SEW)
        solved_end <- end(solved_SEW)
        solved_width <- width(solved_SEW)
        x@nir_list <- lapply(nir_list(x),
            function(nir) shift(restrict(nir, start=solved_start, end=solved_end),
                                1L - solved_start)
        )
        x@width <- solved_width
        if (!normargUseNames(use.names))
            names(x) <- NULL
        x
    }
)

setMethod("narrow", "XVectorList", narrowXVectorList)


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
        if (!is.numeric(width))
            stop("'width' must be numeric")
        if (!is.logical(start) || anyMissing(start))
            stop("'start' must be logical without NA's")
        if (!isTRUEorFALSE(both))
            stop("'both' must be TRUE or FALSE")
        start <- recycleVector(start, length(x))
        width <- recycleVector(width, length(x))
        if (both) {
            ans_start <-
              ifelse(start, start(x) - abs(width), end(x) - abs(width) + 1L)
            ans_width <- 2L * abs(width)
        } else {
            ans_start <-
              ifelse(start, ifelse(width < 0L, start(x), start(x) - width),
                     ifelse(width < 0L, end(x) + width + 1L, end(x) + 1L))
            ans_width <- abs(width)
        }
        x <- update(x, start=ans_start, width=ans_width, check=FALSE)
        if (!normargUseNames(use.names))
            names(x) <- NULL
        x
    }
)

setMethod("flank", "RangesList",
          function(x, width, start = TRUE, both = FALSE, use.names = TRUE)
          {
              lx <- length(x)
              width <- normargAtomicList1(width, IntegerList, lx)
              start <- normargAtomicList1(start, LogicalList, lx)
              mendoapply(flank, x = x, width = width, start = start,
                         MoreArgs = list(both = both, use.names = use.names))
          })

setMethod("flank", "CompressedIRangesList",
          function(x, width, start = TRUE, both = FALSE, use.names = TRUE)
          {
              lx <- length(x)
              eln <- elementLengths(x)
              width <- normargAtomicList2(width, IntegerList, lx, eln)
              start <- normargAtomicList2(start, LogicalList, lx, eln)
              slot(x, "unlistData", check=FALSE) <-
                flank(x@unlistData, width = width, start = start, both = both,
                      use.names = use.names)
              x
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### reflect()
###

setGeneric("reflect", signature="x",
    function(x, bounds, use.names=TRUE)
        standardGeneric("reflect")
)

setMethod("reflect", "Ranges",
    function(x, bounds, use.names=TRUE)
    {
        if (is(x, "NormalIRanges"))
            stop("reflecting a NormalIRanges object is not supported")
        if (!is(bounds, "Ranges"))
            stop("'bounds' must be a Ranges object")
        if (length(x) > 1 && length(bounds) == 0)
            stop("'bounds' is an empty Ranges object")
        if (length(x) < length(bounds))
            bounds <- head(bounds, length(x))
        ans_start <- (2L * start(bounds) + width(bounds) - 1L) - end(x)
        x <- update(x, start=ans_start, width=width(x), check=FALSE)
        if (!normargUseNames(use.names))
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
        if (!is.numeric(width) || anyMissing(width))
            stop("'width' must be a numeric vector without NA's")
        if (!is.integer(width))
            width <- as.integer(width)
        if (anyMissingOrOutside(width, 0L))
            stop("'width' values must be non-negative")
        if (!(is.character(fix) ||
              (is(fix, "Rle") && is.character(runValue(fix)))) || 
            (length(fix) == 0) ||
            (length(setdiff(unique(fix),
                            c("start", "end", "center"))) > 0)) {
            stop("'fix' must be a character vector or character Rle ",
                 "with values in \"start\", \"end\", and \"center\"")
        }
        if (!is(fix, "Rle"))
            fix <- Rle(fix)
        if (length(fix) != lx)
            fix <- rep(fix, length.out = lx)
        ans_width <- recycleVector(width, lx)
        ans_start <- start(x)
        if (!identical(runValue(fix), "start")) {
            fixEnd <- as(fix == "end", "IRanges")
            if (length(fixEnd) > 0) {
                seqselect(ans_start, fixEnd) <-
                  seqselect(ans_start, fixEnd) +
                    (seqselect(width(x), fixEnd) -
                     seqselect(ans_width, fixEnd))
            }
            fixCenter <- as(fix == "center", "IRanges")
            if (length(fixCenter) > 0) {
                seqselect(ans_start, fixCenter) <-
                  seqselect(ans_start, fixCenter) +
                    (seqselect(width(x), fixCenter) -
                     seqselect(ans_width, fixCenter)) %/% 2L
            }
        }
        x <- update(x, start=ans_start, width=ans_width, check=FALSE)
        if (!normargUseNames(use.names))
            names(x) <- NULL
        x
    }
)

setMethod("resize", "RangesList",
          function(x, width, fix = "start", use.names = TRUE)
          {
              lx <- length(x)
              width <- normargAtomicList1(width, IntegerList, lx)
              fix <- normargAtomicList1(fix, CharacterList, lx)
              mendoapply(resize, x = x, width = width, fix = fix,
                         MoreArgs = list(use.names = use.names))
          })

setMethod("resize", "CompressedIRangesList",
          function(x, width, fix = "start", use.names = TRUE)
          {
              lx <- length(x)
              eln <- elementLengths(x)
              width <- normargAtomicList2(width, IntegerList, lx, eln)
              fix <- normargAtomicList2(fix, CharacterList, lx, eln)
              slot(x, "unlistData", check=FALSE) <-
                resize(x@unlistData, width = width, fix = fix,
                       use.names = use.names)
              x
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "restrict" method.
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

### drop.ranges.mode:
###   0L: Ranges in 'x' that are empty after restriction are dropped.
###   1L: Ranges in 'x' that are not overlapping and not even adjacent
###       with the region of restriction are dropped.
###       "Not overlapping and not adjacent" means that they end strictly
###       before start - 1 or start strictly after end + 1.
###       Those that are not overlapping but are however adjacent are kept
###       (and are empty after restriction).
###   2L: All ranges in 'x' are kept after restriction.
###       Ranges
### Note that the only mode compatible with a NormalIRanges object is 0L.
Ranges.restrict <- function(x, start, end, drop.ranges.mode, use.names)
{
    if (!isNumericOrNAs(start))
        stop("'start' must be a vector of integers")
    if (!is.integer(start))
        start <- as.integer(start)
    if (!isNumericOrNAs(end))
        stop("'end' must be a vector of integers")
    if (!is.integer(end))
        end <- as.integer(end)
    if (length(x) != 0L) {
        if (length(start) == 0L || length(start) > length(x))
            stop("invalid 'start' length")
        if (length(end) == 0L || length(end) > length(x))
            stop("invalid 'end' length")
    }
    start <- recycleVector(start, length(x))
    end <- recycleVector(end, length(x))
    use.names <- normargUseNames(use.names)

    ans_start <- start(x)
    ans_end <- end(x)
    if (use.names) ans_names <- names(x) else ans_names <- NULL

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
        keep_it <- !far_too_left
        ans_start <- ans_start[keep_it]
        ans_end <- ans_end[keep_it]
        if (!is.null(ans_names))
            ans_names <- ans_names[keep_it]
        start <- start[keep_it]
        end <- end[keep_it]
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
        keep_it <- !far_too_right
        ans_start <- ans_start[keep_it]
        ans_end <- ans_end[keep_it]
        if (!is.null(ans_names))
            ans_names <- ans_names[keep_it]
        start <- start[keep_it]
        end <- end[keep_it]
    }
    ## Fix 'ans_end'.
    too_right <- !is.na(end) & (ans_end > end)
    ans_end[too_right] <- end[too_right]

    ans_width <- ans_end - ans_start + 1L
    unsafe.update(x, start=ans_start, width=ans_width, names=ans_names)
}

setMethod("restrict", "Ranges",
    function(x, start=NA, end=NA, keep.all.ranges=FALSE, use.names=TRUE)
    {
        if (!isTRUEorFALSE(keep.all.ranges))
            stop("'keep.all.ranges' must be TRUE or FALSE")
        use.names <- normargUseNames(use.names)
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
        Ranges.restrict(x, start, end, drop.ranges.mode, use.names)
    }
)

setMethod("restrict", "RangesList",
          function(x, start = NA, end = NA, keep.all.ranges = FALSE,
                   use.names = TRUE)
          {
              lx <- length(x)
              start <- normargAtomicList1(start, IntegerList, lx)
              end <- normargAtomicList1(end, IntegerList, lx)
              mendoapply(restrict, x, start = start, end = end,
                         MoreArgs = list(keep.all.ranges = keep.all.ranges,
                           use.names = use.names))
          })

setMethod("restrict", "CompressedIRangesList",
          function(x, start = NA, end = NA, keep.all.ranges = FALSE,
                   use.names = TRUE)
          {
              if (!isTRUEorFALSE(keep.all.ranges))
                  stop("'keep.all.ranges' must be TRUE or FALSE")
              if (keep.all.ranges) {
                  lx <- length(x)
                  eln <- elementLengths(x)
                  start <- normargAtomicList2(start, IntegerList, lx, eln)
                  end <- normargAtomicList2(end, IntegerList, lx, eln)
                  slot(x, "unlistData", check=FALSE) <-
                    restrict(x@unlistData, start = start, end = end,
                             keep.all.ranges = keep.all.ranges,
                             use.names = use.names)
              } else
                  x <- callNextMethod()
              x
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "threebands" method.
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

setMethod("threebands", "XVectorList",
    function(x, start=NA, end=NA, width=NA)
    {
        threeranges <- threebands(x@ranges, start=start, end=end, width=width)
        left <- right <- x
        left@ranges <- threeranges$left
        x@ranges <- threeranges$middle
        right@ranges <- threeranges$right
        list(left=left, middle=x, right=right)
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
        if (anyMissing(e2))
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
            update(r, start = ceiling(mid - w/2), width = floor(w), check = FALSE)
        } else {
            if (.Generic == "-") {
                e2 <- -e2
                .Generic <- "+"
            }
            if (.Generic == "+") {
                if (any(-e2*2 > width(e1)))
                    stop("adjustment would result in ranges with negative widths")
                update(e1, start = start(e1) - e2, end = end(e1) + e2, check = FALSE)
            }
        }
    }
)

setMethod("Ops", c("RangesList", "ANY"),
          function(e1, e2)
          {
            for (i in seq_len(length(e1)))
              e1[[i]] <- callGeneric(e1[[i]], e2)
            e1
          })

setMethod("Ops", c("CompressedIRangesList", "ANY"),
          function(e1, e2)
          {
            relist(callGeneric(unlist(e1, use.names = FALSE), e2), e1)
          })

