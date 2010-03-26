### =========================================================================
### Utility functions for creating or modifying Ranges objects
### -------------------------------------------------------------------------
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Intra-interval endomorphisms.
###

setGeneric("flank",
    function(x, width, start=TRUE, both=FALSE, use.names=TRUE)
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
        if (!is(bounds, "Ranges") || length(bounds) != length(x))
            stop("'bounds' must be a Ranges object of length equal to that of 'x'")
        ans_start <- end(bounds) - (end(x) - start(bounds))
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
    function(x, width, fix="start", use.names=TRUE, start=TRUE, symmetric=FALSE)
    {
        lx <- length(x)
        if (!is.numeric(width) || anyMissing(width))
            stop("'width' must be a numeric vector without NA's")
        if (!is.integer(width))
            width <- as.integer(width)
        if (anyMissingOrOutside(width, 0L))
            stop("'width' values must be non-negative")
        if (missing(start) && missing(symmetric)) {
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
        } else {
            fix <- Rle("start", lx)
            if (!missing(start)) {
                if (!is.logical(start) || anyMissing(start))
                    stop("'start' must be a logical vector without NA's")
                warning("argument 'start' is deprecated; use 'fix'.")
                fix <- Rle(ifelse(start, "start", "end"), lx)
            }
            if (!missing(symmetric)) {
                if (!isTRUEorFALSE(symmetric))
                    stop("'symmetric' must be TRUE or FALSE")
                warning("argument 'symmetric' is deprecated; use 'fix'.")
                if (symmetric)
                    fix <- Rle("center", lx)
            }
        }
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
setMethod("restrict", "Ranges",
    function(x, start=NA, end=NA, keep.all.ranges=FALSE, use.names=TRUE)
    {
        start <- normargSingleStartOrNA(start)
        end <- normargSingleEndOrNA(end)
        if (!isTRUEorFALSE(keep.all.ranges))
            stop("'keep.all.ranges' must be TRUE or FALSE")
        use.names <- normargUseNames(use.names)

        ans_start <- start(x)
        ans_end <- end(x)
        if (use.names)
            ans_names <- names(x)
        else
            ans_names <- NULL

        if (!is.na(start)) {
            far_too_left <- ans_end < start - 1L
            if (keep.all.ranges) {
                ans_end[far_too_left] <- start - 1L
            } else {
                keep_it <- !far_too_left
                ans_start <- ans_start[keep_it]
                ans_end <- ans_end[keep_it]
                if (!is.null(ans_names))
                    ans_names <- ans_names[keep_it]
            }
            ## "fix" ans_start
            too_left <- ans_start < start
            ans_start[too_left] <- start
        }
        if (!is.na(end)) {
            far_too_right <- ans_start > end + 1L
            if (keep.all.ranges) {
                ans_start[far_too_right] <- end + 1L
            } else {
                keep_it <- !far_too_right
                ans_start <- ans_start[keep_it]
                ans_end <- ans_end[keep_it]
                if (!is.null(ans_names))
                    ans_names <- ans_names[keep_it]
            }
            ## "fix" ans_end
            too_right <- end < ans_end
            ans_end[too_right] <- end
        }
        ans_width <- ans_end - ans_start + 1L

        update(x, start=ans_start, width=ans_width, names=ans_names,
               check=FALSE)
    }
)

setGeneric("shift", signature="x",
    function(x, shift, use.names=TRUE) standardGeneric("shift")
)
setMethod("shift", "Ranges",
    function(x, shift, use.names=TRUE)
    {
        shift <- normargShift(shift, length(x))
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

### Find objects in the index that overlap those in a query set.
### Deprecated operations
overlap <- function(object, query, maxgap = 0L, multiple = TRUE, ...)
{
    .Deprecated("findOverlaps")
    if (missing(query))
        findOverlaps(object, maxgap = maxgap, multiple = multiple, ...)
    else
        findOverlaps(query, object, maxgap = maxgap, multiple = multiple, ...)
}

countOverlap <- function(object, query)
{
    .Deprecated("countOverlaps")
    countOverlaps(query, object)
}
###

setGeneric("findOverlaps", signature = c("query", "subject"),
    function(query, subject, maxgap = 0L, minoverlap = 1L,
             type = c("any", "start", "end", "within", "equal"),
             select = c("all", "first", "last", "arbitrary"), ...)
        standardGeneric("findOverlaps")
)

setGeneric("countOverlaps",
    function(query, subject, maxgap = 0L, minoverlap = 1L,
             type = c("any", "start", "end", "within", "equal"))
        standardGeneric("countOverlaps")
)

setMethod("countOverlaps", c("Ranges", "Ranges"),
    function(query, subject, maxgap = 0L, minoverlap = 1L,
             type = c("any", "start", "end", "within", "equal"))
    {
        tabulate(queryHits(findOverlaps(query, subject, maxgap = maxgap,
                                        minoverlap = minoverlap, type = type)),
                 length(query))
    }
)

setGeneric("subsetByOverlaps",
    function(query, subject, maxgap = 0L, minoverlap = 1L,
             type = c("any", "start", "end", "within", "equal"))
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

setGeneric("nearest", function(x, subject, ...) standardGeneric("nearest"))

setMethod("nearest", c("Ranges", "RangesORmissing"),
    function(x, subject)
    {
        if (!missing(subject)) {
            ol <- findOverlaps(x, subject, select = "first")
        } else { ## avoid overlapping with self
            subject <- x
            olm <- as.matrix(findOverlaps(x, subject))
            olm <- olm[olm[,1L] != olm[,2L],]
            ol <- olm[,2L][match(seq_len(length(subject)), olm[,1L])]
        }
        x <- x[is.na(ol)]
        before <- precede(x, subject)
        after <- follow(x, subject)
        pre <- (start(subject)[before] - end(x)) < (start(x) - end(subject)[after])
        pre[is.na(pre)] <- is.na(after)[is.na(pre)]
        ol[is.na(ol)] <- ifelse(pre, before, after)
        ol
    }
)

setGeneric("precede", function(x, subject = x, ...) standardGeneric("precede"))

setMethod("precede", c("Ranges", "RangesORmissing"),
    function(x, subject)
    {
        s <- start(subject)
        ord <- NULL
        if (isNotSorted(s)) {
            ord <- orderInteger(s)
            s <- s[ord]
        }
        i <- findInterval(end(x), s) + 1L
        if (!is.null(ord))
            i <- ord[i]
        i[i > length(subject)] <- NA
        i
    }
)

setGeneric("follow", function(x, subject = x, ...) standardGeneric("follow"))

setMethod("follow", c("Ranges", "RangesORmissing"),
    function(x, subject)
    {
        e <- end(subject)
        ord <- NULL
        if (isNotSorted(e)) {
            ord <- orderInteger(e)
            e <- e[ord]
        }
        i <- findInterval(start(x) - 1L, e)
        i[i == 0] <- NA
        if (!is.null(ord))
            i <- ord[i]
        i
    }
)

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
        bins <- .Call("Ranges_disjointBins", start(x), width(x), PACKAGE="IRanges")
        if (!is.null(x_ord)) {
            rev_ord <- integer(length(x_ord))
            rev_ord[x_ord] <- seq_along(rev_ord)
            bins <- bins[rev_ord]
        }
        names(bins) <- names(x)
        bins
    }
)
