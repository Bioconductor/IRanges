### =========================================================================
### Inter-range methods
### -------------------------------------------------------------------------
###


### TODO: We need a ranges() setter for Views objects that provides this
### functionality. Once we have it, use it instead of this.
.set_Views_ranges <- function(x, new_ranges)
{
    new_mcols <- mcols(new_ranges)
    mcols(new_ranges) <- NULL
    BiocGenerics:::replaceSlots(x, ranges=new_ranges,
                                   elementMetadata=new_mcols,
                                   check=FALSE)
}

### NOT exported but used in the GenomicRanges package
global2local_revmap <- function(unlisted_revmap, y, x)
{
    offsets <- rep.int(start(PartitioningByEnd(x)) - 1L, elementNROWS(y))
    unlisted_revmap - offsets
}

### NOT exported but used in the GenomicFeatures package
local2global_revmap <- function(unlisted_revmap, y, x)
{
    offsets <- rep.int(start(PartitioningByEnd(x)) - 1L, elementNROWS(y))
    unlisted_revmap + offsets
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### range()
###

### Always return an IRanges (or NormalIRanges) *instance* whatever Ranges
### derivative the input is, so does NOT act like an endomorphism in general. 
setMethod("range", "Ranges",
    function(x, ..., with.revmap=FALSE, na.rm=FALSE)
    {
        if (!isTRUEorFALSE(with.revmap))
            stop("'with.revmap' must be TRUE or FALSE")
        if (!identical(na.rm, FALSE))
            warning("\"range\" method for Ranges objects ",
                    "ignores the 'na.rm' argument")
        args <- unname(list(x, ...))
        ## TODO: Replace line below with
        ##     args <- lapply(args, ranges)
        ## when ranges() works on Ranges objects.
        args <- lapply(args,
                       function(arg) IRanges(start(arg), width=width(arg)))
        ir <- do.call(c, args)

        ans <- .Call2("IRanges_range", ir, PACKAGE="IRanges")
        if (is(x, "NormalIRanges"))
            ans <- as(ans, "NormalIRanges")
        if (with.revmap){
            mcols(ans) <- DataFrame(revmap=IntegerList(seq_along(ir)))
        }
        ans
    }
)

### Overwrite above method with optimized method for IPos objects.
### Like the above method, return an IRanges instance.
setMethod("range", "IPos",
    function(x, ..., with.revmap=FALSE, ignore.strand=FALSE, na.rm=FALSE)
        callGeneric(stitch_IPos(x), ...,
                    with.revmap=with.revmap, ignore.strand=ignore.strand,
                    na.rm=na.rm)
)

setMethod("range", "RangesList",
    function(x, ..., with.revmap=FALSE, na.rm=FALSE)
    {
        if (length(list(x, ...)) >= 2L)
            x <- merge(x, ...)
        endoapply(x, range, with.revmap=with.revmap)
    }
)

### Equivalent to, but much faster than, 'endoapply(x, range)'.
.range_CompressedIRangesList <- function(x, with.revmap=FALSE)
{
    ## 'x_start' and 'x_end' are CompressedIntegerList objects with the
    ## same shape as 'x'.
    x_start <- start(x)
    x_end <- end(x)
    ## TEMPORARY HACK!
    if (!requireNamespace("XVector", quietly=TRUE))
        stop("the XVector package is required by the \"range\" method ",
             "for CompressedIRangesList objects")
    ## 'sv' and 'ev' are XIntegerViews objects (see XVector package).
    sv <- Views(x_start@unlistData, x_start@partitioning)
    ev <- Views(x_end@unlistData, x_end@partitioning)
    is_not_empty_view <- width(sv) != 0L  # same as 'width(ev) != 0L'
    unlisted_ans <- IRanges(viewMins(sv)[is_not_empty_view],
                            viewMaxs(ev)[is_not_empty_view])
    ans_partitioning <- PartitioningByEnd(cumsum(is_not_empty_view))
    if (with.revmap) {
        x_partitioning <- unname(PartitioningByEnd(x))
        global_revmap <- relist(seq_along(unlist(x, use.names=FALSE)),
                                x_partitioning[width(x_partitioning) != 0L])
        local_revmap <- global2local_revmap(global_revmap, ans_partitioning, x)
        mcols(unlisted_ans)$revmap <- local_revmap
    }
    ans <- relist(unlisted_ans, ans_partitioning)
    names(ans) <- names(x)
    ans
}

setMethod("range", "CompressedIRangesList",
    function(x, ..., with.revmap=FALSE, na.rm=FALSE)
    {
        if (!isTRUEorFALSE(with.revmap))
            stop("'with.revmap' must be TRUE or FALSE")
        if (length(list(x, ...)) >= 2L)
            x <- merge(x, ...)
        .range_CompressedIRangesList(x, with.revmap=with.revmap)
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
    function(x, drop.empty.ranges=FALSE, ...) standardGeneric("reduce")
)

### Always return an IRanges (or NormalIRanges) *instance* whatever Ranges
### derivative the input is, so does NOT act like an endomorphism in general. 
setMethod("reduce", "Ranges",
    function(x, drop.empty.ranges=FALSE, min.gapwidth=1L,
                with.revmap=FALSE,
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
        if (!isTRUEorFALSE(with.revmap))
            stop("'with.revmap' must be TRUE or FALSE")
        if (!isTRUEorFALSE(with.inframe.attrib))
            stop("'with.inframe.attrib' must be TRUE or FALSE")
        C_ans <- .Call2("Ranges_reduce",
                        start(x), width(x),
                        drop.empty.ranges, min.gapwidth,
                        with.revmap,
                        with.inframe.attrib,
                        PACKAGE="IRanges")
        ans <- new2("IRanges", start=C_ans$start,
                               width=C_ans$width,
                               check=FALSE)
        if (is(x, "NormalIRanges"))
            ans <- as(ans, "NormalIRanges")
        if (with.revmap) {
            mcols(ans) <- DataFrame(revmap=IntegerList(C_ans$revmap))
        }
        if (with.inframe.attrib) {
            inframe <- new2("IRanges", start=C_ans$inframe.start,
                                       width=width(x),
                                       check=FALSE)
            attr(ans, "inframe") <- inframe
        }
        ans
    }
)

setMethod("reduce", "Views",
    function(x, drop.empty.ranges=FALSE, min.gapwidth=1L,
                with.revmap=FALSE,
                with.inframe.attrib=FALSE)
    {
        new_ranges <- callGeneric(x@ranges,
                                  drop.empty.ranges=drop.empty.ranges,
                                  min.gapwidth=min.gapwidth,
                                  with.revmap=with.revmap,
                                  with.inframe.attrib=with.inframe.attrib)
        .set_Views_ranges(x, new_ranges)
    }
)

setMethod("reduce", "RangesList",
    function(x, drop.empty.ranges=FALSE, min.gapwidth=1L,
                with.revmap=FALSE,
                with.inframe.attrib=FALSE)
    {
        endoapply(x, reduce, drop.empty.ranges = drop.empty.ranges,
                             min.gapwidth = min.gapwidth,
                             with.revmap=with.revmap,
                             with.inframe.attrib = with.inframe.attrib)
    }
)

### 'with.inframe.attrib' is ignored for now.
### TODO: Support 'with.inframe.attrib=TRUE'.
.reduce_CompressedIRangesList <- function(x, drop.empty.ranges=FALSE,
                                             min.gapwidth=1L,
                                             with.revmap=FALSE,
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
    if (!isTRUEorFALSE(with.revmap))
        stop("'with.revmap' must be TRUE or FALSE")
    if (!identical(with.inframe.attrib, FALSE))
        stop("'with.inframe.attrib' argument not yet supported ",
             "when reducing a CompressedIRangesList object")
    C_ans <- .Call2("CompressedIRangesList_reduce",
                    x, drop.empty.ranges, min.gapwidth, with.revmap,
                    PACKAGE="IRanges")
    unlisted_ans <- new2("IRanges", start=C_ans$start,
                                    width=C_ans$width,
                                    check=FALSE)
    if (with.revmap)
        mcols(unlisted_ans) <- DataFrame(revmap=IntegerList(C_ans$revmap))
    ans_partitioning <- PartitioningByEnd(C_ans$breakpoints)
    names(ans_partitioning) <- names(x)
    relist(unlisted_ans, ans_partitioning)
}

setMethod("reduce", "CompressedIRangesList", .reduce_CompressedIRangesList)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### gaps()
###

setGeneric("gaps", signature="x",
    function(x, start=NA, end=NA) standardGeneric("gaps")
)

### Always return an IRanges (or NormalIRanges) *instance* whatever Ranges
### derivative the input is, so does NOT act like an endomorphism in general. 
setMethod("gaps", "Ranges",
    function(x, start=NA, end=NA)
    {
        start <- S4Vectors:::normargSingleStartOrNA(start)
        end <- S4Vectors:::normargSingleEndOrNA(end)
        C_ans <- .Call2("IRanges_gaps",
                        start(x), width(x), start, end,
                        PACKAGE="IRanges")
        ans <- new2("IRanges", start=C_ans$start,
                               width=C_ans$width,
                               check=FALSE)
        if (is(x, "NormalIRanges"))
            ans <- as(ans, "NormalIRanges")
        ans
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
        new_ranges <- gaps(x@ranges, start=start, end=end)
        .set_Views_ranges(x, new_ranges)
    }
)

.gaps_RangesList <- function(x, start=NA, end=NA)
{
    x_len <- length(x)
    if (!S4Vectors:::isNumericOrNAs(start))
        stop("'start' must be an integer vector or NA")
    if (!is.integer(start))
        start <- as.integer(start)
    if (!S4Vectors:::isNumericOrNAs(end))
        stop("'end' must be an integer vector or NA")
    if (!is.integer(end))
        end <- as.integer(end)
    start <- IntegerList(as.list(S4Vectors:::recycleVector(start, x_len)))
    end <- IntegerList(as.list(S4Vectors:::recycleVector(end, x_len)))
    mendoapply(gaps, x, start = start, end = end)
}

setMethod("gaps", "RangesList", .gaps_RangesList)

.gaps_CompressedIRangesList <- function(x, start=NA, end=NA)
{
    ## Normalize 'start'.
    if (!S4Vectors:::isNumericOrNAs(start))
        stop("'start' must be an integer vector or NA")
    if (!is.integer(start))
        start <- as.integer(start)
    if (length(start) != 1L)
        start <- S4Vectors:::V_recycle(start, x,
                                       x_what="start", skeleton_what="x")
    ## Normalize 'end'.
    if (!S4Vectors:::isNumericOrNAs(end))
        stop("'end' must be an integer vector or NA")
    if (!is.integer(end))
        end <- as.integer(end)
    if (length(end) != 1L)
        end <- S4Vectors:::V_recycle(end, x,
                                     x_what="end", skeleton_what="x")

    chunksize <- 10000000L
    if (length(x) <= chunksize) {
        ## Process all at once.
        ans <- .Call2("CompressedIRangesList_gaps",
                      x, start, end,
                      PACKAGE="IRanges")
        return(ans)
    }

    ## Process by chunk.
    verbose <- getOption("verbose", default=FALSE)
    chunks <- as(breakInChunks(length(x), chunksize), "IRanges")
    ans_chunks <- lapply(seq_along(chunks),
        function(i) {
            if (verbose)
                cat("Processing chunk #", i, "/", length(chunks), " ... ",
                    sep="")
            chunk <- chunks[i]
            x_chunk <- extractROWS(x, chunk)
            start_chunk <- if (length(start) == 1L) start
                           else extractROWS(start, chunk)
            end_chunk <- if (length(end) == 1L) end
                         else extractROWS(end, chunk)
            ans_chunk <- .gaps_CompressedIRangesList(x_chunk,
                                                     start=start_chunk,
                                                     end=end_chunk)
            if (verbose)
                cat("OK\n")
            ans_chunk
        })
    do.call(c, ans_chunks)
}

setMethod("gaps", "CompressedIRangesList", .gaps_CompressedIRangesList)

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

### Always return an IRanges *instance* whatever Ranges derivative the input
### is, so does NOT act like an endomorphism in general.
setMethod("disjoin", "Ranges",
    function(x, with.revmap=FALSE)
    {
        if (!isTRUEorFALSE(with.revmap))
            stop("'with.revmap' must be TRUE or FALSE")
        ## starts: original starts and end+1 when inside another interval
        ## ends: original ends and start-1 when inside another interval
        starts <- unique(start(x))
        ends <- unique(end(x))
        adj_start <- head(sort(unique(c(starts, ends + 1L))), -1L)
        adj_end <- tail(sort(unique(c(ends, starts - 1L))), -1L)
        adj_width <- adj_end - adj_start + 1L
        adj <- new2("IRanges", start=adj_start,
                               width=adj_width,
                               check=FALSE)
        adj <- subsetByOverlaps(adj, x)
        if (with.revmap)
             mcols(adj)$revmap <- as(sort(findOverlaps(adj, x)),"List")
        adj
    }
)

### Basically a no-op but returns a NormalIRanges *instance* for consistency
### with how the other inter-range transformations (range(), reduce(), gaps())
### behave on a NormalIRanges object.
setMethod("disjoin", "NormalIRanges", function(x) as(x, "NormalIRanges"))

setMethod("disjoin", "RangesList",
    function(x, with.revmap=FALSE)
        endoapply(x, disjoin, with.revmap=with.revmap)
)

setMethod("disjoin", "CompressedIRangesList",
          function(x, with.revmap=FALSE, ...)
          {
              if (!isTRUEorFALSE(with.revmap))
                  stop("'with.revmap' must be TRUE or FALSE")        
              .wunlist <- function(x)
                  ## unlist CompressedIntegerList, with integer(0) as 0
              {
                  w <- integer(length(x))
                  w[elementNROWS(x) != 0L] <- unlist(x, use.names=FALSE)
                  w
              }
       
              rng <- range(x)
              if (sum(.wunlist(width(rng) + 1)) > .Machine$integer.max)
                  return(endoapply(x, disjoin, with.revmap=with.revmap, ...))

              ## localize coordinates
              off0 <- head(.wunlist(width(rng) + 1L), -1L)
              offset <- c(1L, cumsum(off0)) - .wunlist(start(rng))
              local <- unlist(shift(x, offset), use.names=FALSE)

              ## disjoin
              d <- disjoin(local, with.revmap=with.revmap, ...)
              vec <- unlist(start(shift(rng, offset)), use.names=FALSE)
              lvls <- factor(seq_along(x))
              lvls0 <- lvls[elementNROWS(rng) != 0]
              f <- lvls0[findInterval(start(d), vec)]
              ans <- split(d, f)

              ## globalize coordinates
              ans <- shift(ans, -offset)

              ## localize 'revmap'
              if (with.revmap) {
                  unlisted_ans <- unlist(ans, use.names=FALSE)
                  global_revmap <- mcols(unlisted_ans)$revmap
                  local_revmap <- global2local_revmap(global_revmap, ans, x)
                  mcols(unlisted_ans)$revmap <- local_revmap
                  ans <- relist(unlisted_ans, ans)
              }

              names(ans) <- names(x)
              ans
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### isDisjoint()
###

setGeneric("isDisjoint", function(x, ...) standardGeneric("isDisjoint"))

setMethod("isDisjoint", "Ranges",
    function(x)
    {
        x_len <- length(x)
        if (x_len < 2L)
            return(TRUE)
        x_start <- start(x)
        x_end <- end(x)
        oo <- order(x)
        start2 <- x_start[oo]
        end2 <- x_end[oo]
        all(start2[-1L] > end2[-x_len])
    }
)

### Overwrite above method with optimized method for IPos objects.
setMethod("isDisjoint", "IPos", function(x) callGeneric(stitch_IPos(x)))

setMethod("isDisjoint", "NormalIRanges", function(x) TRUE)

setMethod("isDisjoint", "RangesList",
    function(x) vapply(x, isDisjoint, logical(1))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### disjointBins()
###

## make intervals disjoint by segregating them into separate Ranges
setGeneric("disjointBins", function(x, ...) standardGeneric("disjointBins"))

setMethod("disjointBins", "Ranges",
    function(x)
    {
        x_ord <- NULL
        if (S4Vectors:::isNotSorted(start(x))) { # minimize work for sorted ranges (common)
            x_ord <- order(x)
            x <- x[x_ord]
        }
        bins <- .Call2("Ranges_disjointBins", start(x), width(x),
                       PACKAGE="IRanges")
        if (!is.null(x_ord)) {
            rev_ord <- integer(length(x_ord))
            rev_ord[x_ord] <- seq_along(rev_ord)
            bins <- bins[rev_ord]
        }
        names(bins) <- names(x)
        bins
    }
)

### Overwrite above method with trivial method for NormalIRanges objects.
setMethod("disjointBins", "NormalIRanges", function(x) rep.int(1L, length(x)))

setMethod("disjointBins", "RangesList",
    function(x) as(lapply(x, disjointBins), "IntegerList")
)

