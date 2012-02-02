### =========================================================================
### Utility functions for creating or modifying Ranges objects
### -------------------------------------------------------------------------
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Intra-interval endomorphisms.
###

setGeneric("flank",
    function(x, width, start=TRUE, both=FALSE, use.names=TRUE, ...)
        standardGeneric("flank")
)
setMethod("flank", "Ranges",
    function(x, width, start=TRUE, both=FALSE, use.names=TRUE)
    {
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

setGeneric("narrow", signature="x",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
        standardGeneric("narrow")
)
setMethod("narrow", "Ranges",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
    {
        solved_SEW <- solveUserSEW(width(x), start=start, end=end, width=width)
        ans_start <- start(x) + start(solved_SEW) - 1L
        ans_width <- width(solved_SEW)
        x <- update(x, start=ans_start, width=ans_width, check=FALSE)
        if (!normargUseNames(use.names))
            names(x) <- NULL
        x
    }
)

setGeneric("reflect",
    function(x, bounds, use.names=TRUE)
        standardGeneric("reflect")
)
setMethod("reflect", "Ranges",
    function(x, bounds, use.names=TRUE)
    {
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

setGeneric("resize", signature="x",
    function(x, width, fix="start", use.names=TRUE, ...)
        standardGeneric("resize")
)
setMethod("resize", "Ranges",
    function(x, width, fix="start", use.names=TRUE)
    {
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
        if (keep.all.ranges)
            drop.ranges.mode <- 2L
        else
            drop.ranges.mode <- 1L
        Ranges.restrict(x, start, end, drop.ranges.mode, use.names)
    }
)

setGeneric("shift", signature="x",
    function(x, shift=0L, use.names=TRUE) standardGeneric("shift")
)
setMethod("shift", "Ranges",
    function(x, shift=0L, use.names=TRUE)
    {
        shift <- recycleIntegerArg(shift, "shift", length(x))
        x <- update(x, start=start(x) + shift, width=width(x), check=FALSE)
        if (!normargUseNames(use.names))
            names(x) <- NULL
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Inter-interval endomorphisms.
###

setGeneric("disjoin", function(x, ...) standardGeneric("disjoin"))
setMethod("disjoin", "Ranges",
    function(x)
    {
        ## starts: original starts and end+1 when inside another interval
        ## ends: original ends and start-1 when inside another interval
        starts <- unique(start(x))
        ends <- unique(end(x))
        adj_start <- sort(unique(c(starts, ends + 1L)))
        adj_end <- sort(unique(c(ends, starts - 1L)))
        adj <- update(x, start=head(adj_start, -1L), end=tail(adj_end, -1L),
                      names=NULL, check=FALSE)
        subsetByOverlaps(adj,  x)
    }
)

setGeneric("gaps", signature="x",
    function(x, start=NA, end=NA) standardGeneric("gaps")
)
setMethod("gaps", "Ranges",
    function(x, start=NA, end=NA)
    {
        ir <- as(x, "IRanges")
        y <- gaps(ir, start=start, end=end)
        as(y, class(x))
    }
)

setMethod("range", "Ranges",
    function(x, ..., na.rm)
    {
        args <- unname(list(x, ...))
        if (!all(sapply(args, is, "Ranges")))
            stop("all arguments in '...' must be Ranges objects")
        x <- do.call(c, args)
        if (length(x) == 0)
            y <- IRanges()
        else
            y <- IRanges(min(start(x)), max(end(x)))
        as(y, class(x))
    }
)

setGeneric("reduce", signature="x",
    function(x, ...)
        standardGeneric("reduce")
)

setMethod("reduce", "Ranges",
    function(x, drop.empty.ranges=FALSE, min.gapwidth=1L,
             with.inframe.attrib=FALSE)
    {
        ir <- as(x, "IRanges")
        y <- reduce(ir,
                    drop.empty.ranges=drop.empty.ranges,
                    min.gapwidth=min.gapwidth,
                    with.inframe.attrib=with.inframe.attrib)
        as(y, class(x))
    }
)

setGeneric("threebands", signature="x",
    function(x, start=NA, end=NA, width=NA)
        standardGeneric("threebands")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### More operations.
###

### Find objects in the query that overlap those in a subject set.

setGeneric("findOverlaps", signature = c("query", "subject"),
    function(query, subject, maxgap = 0L, minoverlap = 1L,
             type = c("any", "start", "end", "within", "equal"),
             select = c("all", "first", "last", "arbitrary"), ...)
        standardGeneric("findOverlaps")
)

setGeneric("countOverlaps",
    function(query, subject, maxgap = 0L, minoverlap = 1L,
             type = c("any", "start", "end", "within", "equal"), ...)
        standardGeneric("countOverlaps")
)

setMethod("countOverlaps", c("Ranges", "Ranges"),
    function(query, subject, maxgap = 0L, minoverlap = 1L,
             type = c("any", "start", "end", "within", "equal"))
    {
        counts <- queryHits(findOverlaps(query, subject, maxgap = maxgap,
                                         minoverlap = minoverlap, type = type))
        structure(tabulate(counts, length(query)), names=names(query))
    }
)

setGeneric("subsetByOverlaps",
    function(query, subject, maxgap = 0L, minoverlap = 1L,
             type = c("any", "start", "end", "within", "equal"), ...)
        standardGeneric("subsetByOverlaps")
)

setMethod("subsetByOverlaps", c("Ranges", "Ranges"),
    function(query, subject, maxgap = 0L, minoverlap = 1L,
             type = c("any", "start", "end", "within", "equal"))
    {
        type <- match.arg(type)
        query[!is.na(findOverlaps(query, subject, maxgap = maxgap,
                                  minoverlap = minoverlap, type = type,
                                  select = "arbitrary"))]
    }
)

setMethod("%in%", c("Ranges", "Ranges"),
    function(x, table)
    {
        if (!is(x, "IRanges"))
            x <- as(x, "IRanges")
        subject <- IntervalTree(table)
        if (isNotSorted(start(x))) { ## x must be sorted
            x_ord <- order(x)
            x <- x[x_ord]
        } else {
            x_ord <- seq_len(length(x))
        }
        IRanges:::.IntervalTreeCall(subject, "overlap_any", x, x_ord)
    }
)

setGeneric("match",
    function(x, table, nomatch = NA_integer_, incomparables = NULL)
        standardGeneric("match")
)

setMethod("match", c("Ranges", "Ranges"),
    function(x, table, nomatch = NA_integer_, incomparables = NULL)
    {
        if (length(nomatch) != 1)
            stop("'nomatch' must be of length 1") 
        ans <- findOverlaps(x, table, select = "first")
        if (!is.na(nomatch) && anyMissing(ans))
            ans[is.na(ans)] <- nomatch
        ans
    }
)

setClassUnion("RangesORmissing", c("Ranges", "missing"))

.hitsMatrixToVector <- function(hitsMatrix, queryLength) {
  hitsMatrix <-
    hitsMatrix[diffWithInitialZero(hitsMatrix[,1L,drop=TRUE]) != 0L,,
                drop=FALSE]
  ans <- rep.int(NA_integer_, queryLength)
  ans[hitsMatrix[,1L,drop=TRUE]] <- hitsMatrix[,2L,drop=TRUE]
  ans
}

.vectorToHits <- function(i, srle, ord) {
  lx <- length(i)
  v <- !is.na(i)
  i <- i[v]
  w <- width(srle)[i]
  subj <- as.integer(IRanges(start(srle)[i], width=w))
  m <- cbind(queryHits = rep(seq(lx)[v], w),
             subjectHits = if (!is.null(ord)) ord[subj] else subj)
  if (!is.null(ord))
    m <- m[orderIntegerPairs(m[,1L], m[,2L]),,drop=FALSE]
  ## unname() required because in case 'm' has only 1 row
  ## 'm[ , 1L]' and 'm[ , 2L]' will return a named atomic vector
  new("Hits", queryHits = unname(m[ , 1L]), subjectHits = unname(m[ , 2L]),
              queryLength = lx, subjectLength = length(srle))
}

setGeneric("nearest", function(x, subject, ...) standardGeneric("nearest"))

setMethod("nearest", c("Ranges", "RangesORmissing"),
          function(x, subject, select = c("arbitrary", "all"))
          {
            select <- match.arg(select)
            if (!missing(subject)) {
              ol <- findOverlaps(x, subject, select = select)
            } else {
              subject <- x
              ol <- findOverlaps(x, select = select, ignoreSelf = TRUE)
            }
            if (select == "all") {
              m <- as.matrix(ol)
              olv <- .hitsMatrixToVector(m, length(x))
            } else olv <- ol
            x <- x[is.na(olv)]
            before <- precede(x, subject,
                              if (select == "all") "all" else "first")
            after <- follow(x, subject,
                            if (select == "all") "all" else "last")
            if (select == "all") {
              before_m <- as.matrix(before)
              before <- .hitsMatrixToVector(before_m, length(x))
              after_m <- as.matrix(after)
              after <- .hitsMatrixToVector(after_m, length(x))
            }
            leftdist <- (start(subject)[before] - end(x))
            rightdist <- (start(x) - end(subject)[after])
            left <- leftdist < rightdist
            left[is.na(left)] <- is.na(after)[is.na(left)]
            if (select == "all") {
              filterMatchMatrix <- function(m, i) {
                qrle <- Rle(m[,1L])
                qstart <- qend <- integer(length(i))
                qstart[runValue(qrle)] <- start(qrle)
                qend[runValue(qrle)] <- end(qrle)
                rows <- as.integer(IRanges(qstart[i], qend[i]))
                m <- m[rows,,drop=FALSE]
                m[,1L] <- map[m[,1L]]
                m
              }
              map <- which(is.na(olv))
              right <- !left
              left[leftdist == rightdist] <- TRUE
              m <- rbind(m, filterMatchMatrix(before_m, left),
                         filterMatchMatrix(after_m, right))
              m <- m[orderIntegerPairs(m[,1L], m[,2L]),, drop=FALSE]
              ## unname() required because in case 'm' has only 1 row
              ## 'm[ , 1L]' and 'm[ , 2L]' will return a named atomic vector
              ol@queryHits <- unname(m[ , 1L])
              ol@subjectHits <- unname(m[ , 2L])
            } else {
              olv[is.na(olv)] <- ifelse(left, before, after)
              ol <- olv
            }
            ol
          })

setGeneric("precede", function(x, subject = x, ...) standardGeneric("precede"))

setMethod("precede", c("Ranges", "RangesORmissing"),
    function(x, subject, select = c("first", "all"))
    {
      select <- match.arg(select)
      s <- start(subject)
      ord <- NULL
      if (isNotSorted(s)) {
        ord <- orderInteger(s)
        s <- s[ord]
      }
      if (select == "all") {
        srle <- Rle(s)
        s <- runValue(srle)
      }
      i <- findInterval(end(x), s) + 1L
      i[i > length(s)] <- NA
      if (select == "all") {
        .vectorToHits(i, srle, ord)
      } else {
        if (!is.null(ord))
          i <- ord[i]
        i
      }
    }
)

setGeneric("follow", function(x, subject = x, ...) standardGeneric("follow"))

setMethod("follow", c("Ranges", "RangesORmissing"),
    function(x, subject, select = c("last", "all"))
    {
      select <- match.arg(select)
      e <- end(subject)
      ord <- NULL
      if (isNotSorted(e)) {
        ord <- orderInteger(e)
        e <- e[ord]
      }
      if (select == "all") {
        srle <- Rle(e)
        e <- runValue(srle)
      }
      i <- findInterval(start(x) - 1L, e)
      i[i == 0] <- NA        
      if (select == "all") {
        .vectorToHits(i, srle, ord)
      } else {
        if (!is.null(ord))
          i <- ord[i]
        i
      }
    }
)

setGeneric("distanceToNearest",
           function(x, subject = x, ...) standardGeneric("distanceToNearest"))

setMethod("distanceToNearest", c("Ranges", "RangesORmissing"),
          function(x, subject, select = c("arbitrary", "all"))
          {
            select <- match.arg(select)
            if (missing(subject)) {
              subject <- x
              x_nearest <- nearest(x, select = select)
            } else x_nearest <- nearest(x, subject, select = select)
            if (select == "arbitrary")
              x_nearest <- cbind(queryHits = seq(length(x)),
                                 subjectHits = x_nearest)
            else x_nearest <- as.matrix(x_nearest)
            x <- x[x_nearest[,1]]
            subject <- subject[x_nearest[,2]]
            DataFrame(x_nearest, distance = distance(x, subject))
          })

setGeneric("distance",
           function(x, y, ...) standardGeneric("distance"))

setMethod("distance", c("Ranges", "Ranges"), function(x, y) {
  if (length(x) != length(y))
    stop("'x' and 'y' must have the same length")
  ans_end_plus1 <- pmax.int(start(x), start(y))
  ans_start <- pmin.int(end(x), end(y))
  ans <- ans_end_plus1 - ans_start
  pmax(ans, if (length(x)) 0L else integer())
})


## zooming (symmetrically scales the width)
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

## make intervals disjoint by segregating them into separate Ranges
setGeneric("disjointBins", function(x, ...) standardGeneric("disjointBins"))
setMethod("disjointBins", "Ranges",
    function(x)
    {
        x_ord <- NULL
        if (isNotSorted(start(x))) { # minimize work for sorted ranges (common)
            x_ord <- order(x)
            x <- x[x_ord]
        }
        bins <- .Call2("Ranges_disjointBins", start(x), width(x), PACKAGE="IRanges")
        if (!is.null(x_ord)) {
            rev_ord <- integer(length(x_ord))
            rev_ord[x_ord] <- seq_along(rev_ord)
            bins <- bins[rev_ord]
        }
        names(bins) <- names(x)
        bins
    }
)
