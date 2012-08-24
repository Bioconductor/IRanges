### =========================================================================
### Inter-range methods
### -------------------------------------------------------------------------
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### range()
###

setMethod("range", "Ranges",
    function(x, ..., na.rm=FALSE)
    {
        if (!identical(na.rm, FALSE))
            warning("'na.rm' argument is ignored")
        args <- unname(list(x, ...))
        if (length(args) == 0L)  # should never happen
            return(IRanges())
        if (!all(sapply(args, is, "Ranges")))
            stop("all arguments in '...' must be Ranges objects")
        xx <- do.call(c, lapply(args, as, "IRanges"))
        y <- .Call2("IRanges_range", xx, PACKAGE="IRanges")
        as(y, class(args[[1L]]))
    }
)

setMethod("range", "RangesList",
    function(x, ..., na.rm=FALSE)
    {
        if (length(list(x, ...)) >= 2L)
            x <- merge(x, ...)
        endoapply(x, range)
    }
)

### Equivalent to, but much faster than, 'endoapply(x, range)'.
.CompressedIRangesList.range <- function(x)
{
    ## 'x_start' and 'x_end' are CompressedIntegerList objects with the
    ## same shape as 'x'.
    x_start <- start(x)
    x_end <- end(x)
    ## 'sv' and 'ev' are XIntegerViews objects.
    sv <- Views(x_start@unlistData, x_start@partitioning)
    ev <- Views(x_end@unlistData, x_end@partitioning)
    is_not_empty_view <- width(sv) != 0L  # same as 'width(ev) != 0L'
    ans_unlistData <- IRanges(viewMins(sv)[is_not_empty_view],
                              viewMaxs(ev)[is_not_empty_view])
    ans_partitioning <- new2("PartitioningByEnd",
                             end=cumsum(is_not_empty_view), check=FALSE)
    ans <- new2("CompressedIRangesList",
                unlistData=ans_unlistData,
                partitioning=ans_partitioning,
                check=FALSE)
    names(ans) <- names(x)
    mcols(ans) <- mcols(x)
    ans
}

setMethod("range", "CompressedIRangesList",
    function(x, ..., na.rm=FALSE)
    {
        if (length(list(x, ...)) >= 2L)
            x <- merge(x, ...)
        .CompressedIRangesList.range(x)
    }
)

setMethod("range", "RangedData", function(x, ..., na.rm) {
  args <- list(x, ...)
  rangeLists <- lapply(args, ranges)
  do.call(range, rangeLists)
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### reduce()
###

setGeneric("reduce", signature="x",
    function(x, ...)
        standardGeneric("reduce")
)

setMethod("reduce", "IRanges",
    function(x, drop.empty.ranges=FALSE, min.gapwidth=1L,
             with.inframe.attrib=FALSE)
    {
        if (!isTRUEorFALSE(drop.empty.ranges))
            stop("'drop.empty.ranges' must be TRUE or FALSE")
        if (!isSingleNumber(min.gapwidth))
            stop("'min.gapwidth' must be a single integer")
        if (!is.integer(min.gapwidth))
            min.gapwidth <- as.integer(min.gapwidth)
        if (min.gapwidth < 0L)
            stop("'min.gapwidth' must be non-negative")
        if (!isTRUEorFALSE(with.inframe.attrib))
            stop("'with.inframe.attrib' must be TRUE or FALSE")
        C_ans <- .Call2("IRanges_reduce",
                        x, drop.empty.ranges, min.gapwidth, with.inframe.attrib,
                        PACKAGE="IRanges")
        ans <- unsafe.update(x, start=C_ans$start, width=C_ans$width, names=NULL)
        if (with.inframe.attrib) {
            inframe <- new2("IRanges", start=C_ans$inframe.start,
                                       width=width(x), check=FALSE)
            attr(ans, "inframe") <- inframe
        }
        ans
    }
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

setMethod("reduce", "Views",
    function(x, drop.empty.ranges=FALSE, min.gapwidth=1L,
             with.inframe.attrib=FALSE)
    {
        x@ranges <- reduce(ranges(x),
                           drop.empty.ranges=drop.empty.ranges,
                           min.gapwidth=min.gapwidth,
                           with.inframe.attrib=with.inframe.attrib)
        x
    }
)

setMethod("reduce", "RangesList",
          function(x, drop.empty.ranges = FALSE, min.gapwidth = 1L,
                   with.inframe.attrib = FALSE)
          endoapply(x, reduce, drop.empty.ranges = drop.empty.ranges,
                    min.gapwidth = min.gapwidth,
                    with.inframe.attrib = with.inframe.attrib))

### 'with.inframe.attrib' is ignored for now.
### TODO: Support 'with.inframe.attrib=TRUE'.
setMethod("reduce", "CompressedIRangesList",
          function(x, drop.empty.ranges = FALSE, min.gapwidth = 1L,
                   with.inframe.attrib = FALSE)
          {
              if (!isTRUEorFALSE(drop.empty.ranges))
                  stop("'drop.empty.ranges' must be TRUE or FALSE")
              if (!isSingleNumber(min.gapwidth))
                  stop("'min.gapwidth' must be a single integer")
              if (!is.integer(min.gapwidth))
                  min.gapwidth <- as.integer(min.gapwidth)
              if (min.gapwidth < 0L)
                  stop("'min.gapwidth' must be non-negative")
              if (!identical(with.inframe.attrib, FALSE))
                  stop("'with.inframe.attrib' argument not yet supported ",
                       "when reducing a CompressedIRangesList object")
              .Call2("CompressedIRangesList_reduce",
                    x, drop.empty.ranges, min.gapwidth,
                    PACKAGE="IRanges")
          })

setMethod("reduce", "RangedData",
          function(x, by = character(), drop.empty.ranges=FALSE,
                   min.gapwidth=1L, with.inframe.attrib=FALSE)
          {
            if (!isTRUEorFALSE(drop.empty.ranges))
                stop("'drop.empty.ranges' must be TRUE or FALSE")
            if (!isSingleNumber(min.gapwidth))
                stop("'min.gapwidth' must be a single integer")
            if (!is.integer(min.gapwidth))
                min.gapwidth <- as.integer(min.gapwidth)
            if (min.gapwidth < 0L)
                stop("'min.gapwidth' must be non-negative")
            FUN <- function(y) {
              name <- names(y)
              ranges <- ranges(y)[[1L]]
              values <- values(y)[[1L]]
              inds <-
                unname(split(seq_len(nrow(values)), lapply(values, as.vector)))
              rlist <-
                lapply(inds, function(i) {
                         rngs <-
                           reduce(ranges[i],
                                  drop.empty.ranges=drop.empty.ranges,
                                  min.gapwidth=min.gapwidth,
                                  with.inframe.attrib=with.inframe.attrib)
                         list(ranges = rngs,
                              values =
                              values[rep(i, length.out = length(rngs)), ,
                                     drop=FALSE])
                       })
              ranges <-
                IRangesList(do.call(c, lapply(rlist, "[[", "ranges")))
              names(ranges) <- name
              values <-
                SplitDataFrameList(do.call(rbind,
                                           lapply(rlist, "[[", "values")))
              names(values) <- name
              new2(class(y), ranges = ranges, values = values, check = FALSE)
            }
            if (ncol(x) == 0 || length(by) == 0) {
              ranges <-
                reduce(ranges(x),
                       drop.empty.ranges = drop.empty.ranges,
                       min.gapwidth = min.gapwidth,
                       with.inframe.attrib = with.inframe.attrib)
              listData <- new2("DataFrame", nrows=sum(elementLengths(ranges)),
                               check=FALSE)
              end <- cumsum(elementLengths(ranges))
              names(end) <- names(ranges)
              partitioning <- PartitioningByEnd(end)
              initialize(x,
                         ranges = ranges,
                         values = relist(listData, partitioning))
            } else {
              endoapply(x[,by], FUN)
            }
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### gaps()
###

setGeneric("gaps", signature="x",
    function(x, start=NA, end=NA) standardGeneric("gaps")
)

setMethod("gaps", "IRanges",
    function(x, start=NA, end=NA)
    {
        start <- normargSingleStartOrNA(start)
        end <- normargSingleEndOrNA(end)
        C_ans <- .Call2("IRanges_gaps", x, start, end, PACKAGE="IRanges")
        initialize(x, start=C_ans$start, width=C_ans$width, NAMES=NULL,
                   elementMetadata=NULL)
    }
)

setMethod("gaps", "Ranges",
    function(x, start=NA, end=NA)
    {
        ir <- as(x, "IRanges")
        y <- gaps(ir, start=start, end=end)
        as(y, class(x))
    }
)

setMethod("gaps", "Views",
    function(x, start=NA, end=NA)
    {
        if (!isSingleNumberOrNA(start))
            stop("'start' must be a single integer")
        if (!is.integer(start))
            start <- as.integer(start)
        if (!isSingleNumberOrNA(end))
            stop("'end' must be a single integer")
        if (!is.integer(end))
            end <- as.integer(end)
        if (is.na(start))
            start <- 1L
        if (is.na(end))
            end <- length(subject(x))
        x@ranges <- gaps(ranges(x), start=start, end=end)
        x
    }
)

setMethod("gaps", "RangesList",
          function(x, start = NA, end = NA)
          {
              lx <- length(x)
              if (!isNumericOrNAs(start))
                  stop("'start' must be an integer vector or NA")
              if (!is.integer(start))
                  start <- as.integer(start)
              if (!isNumericOrNAs(end))
                  stop("'end' must be an integer vector or NA")
              if (!is.integer(end))
                  end <- as.integer(end)
              start <- IntegerList(as.list(recycleVector(start, lx)))
              end <- IntegerList(as.list(recycleVector(end, lx)))
              mendoapply(gaps, x, start = start, end = end)
          })

setMethod("gaps", "CompressedIRangesList",
          function(x, start = NA, end = NA)
          {
              lx <- length(x)
              if (!isNumericOrNAs(start))
                  stop("'start' must be an integer vector or NA")
              if (!is.integer(start))
                  start <- as.integer(start)
              if (!isNumericOrNAs(end))
                  stop("'end' must be an integer vector or NA")
              if (!is.integer(end))
                  end <- as.integer(end)
              if ((length(start) != 1) || (length(end) != 1)) {
                  start <- recycleVector(start, lx)
                  end <- recycleVector(end, lx)
              }
              .Call2("CompressedIRangesList_gaps", x, start, end,
                    PACKAGE="IRanges")
          })

### 'start' and 'end' are ignored.
setMethod("gaps", "MaskCollection",
    function(x, start=NA, end=NA)
    {
        start <- 1L
        end <- width(x)
        x@nir_list <- lapply(nir_list(x),
            function(nir) gaps(nir, start=start, end=end)
        )
        x@NAMES <- as.character(NA)
        x@desc <- as.character(NA)
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### disjoin()
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

setMethod("disjoin", "RangesList", function(x) endoapply(x, disjoin))

setMethod("disjoin", "CompressedIRangesList",
          function(x, ...)
          {
              .wunlist <- function(x)
                  ## unlist CompressedIntegerList, with integer(0) as 0
              {
                  w <- integer(length(x))
                  w[elementLengths(x) != 0L] <- unlist(x, use.names=FALSE)
                  w
              }

              rng <- range(x)
              if (sum(.wunlist(width(rng) + 1)) > .Machine$integer.max)
                  return(endoapply(x, disjoin,  ...))

              ## localize coordinates
              off0 <- head(.wunlist(width(rng) + 1L), -1L)
              offset <- c(1L, cumsum(off0)) - .wunlist(start(rng))
              local <- unlist(shift(x, offset), use.names=FALSE)

              ## disjoin
              lvls <- names(x)
              if (is.null(lvls))
                  lvls <- seq_along(x)
              d <- disjoin(local, ...)
              vec <- unlist(start(shift(rng, offset)), use.names=FALSE)
              lvls0 <- lvls[elementLengths(rng) != 0]
              f <- lvls0[findInterval(start(d), vec)]
              d <- split(d, factor(f, levels=lvls))
              if (is.null(names(x)))
                  names(d) <- NULL

              ## globalize coordinates
              shift(d, -offset)
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### isDisjoint()
###

setGeneric("isDisjoint", function(x, ...) standardGeneric("isDisjoint"))

setMethod("isDisjoint", "Ranges",
    function(x)
    {
        x <- x[width(x) > 0L]
        if (length(x) < 2L)
            return(TRUE)
        starts <- start(x)
        startord <- orderInteger(starts)
        all(starts[startord][-1L] - end(x)[startord][-length(x)] >= 1L)
    }
)

setMethod("isDisjoint", "RangesList",
          function(x) unlist(lapply(x, isDisjoint)))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### disjointBins()
###

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

setMethod("disjointBins", "RangesList", function(x) seqapply(x, disjointBins))

