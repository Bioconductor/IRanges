### =========================================================================
### windows()
### -------------------------------------------------------------------------


### windows() is a "parallel" version of window() for list-like objects. That
### is, it does 'mendoapply(window, x, start, end, width)' but uses a fast
### implementation.
setGeneric("windows", signature="x",
    function(x, start=NA, end=NA, width=NA) standardGeneric("windows")
)

### NOT exported.
### Low-level utility used by various "windows" methods.
make_IRanges_from_windows_args <- function(x, start=NA, end=NA, width=NA)
{
    x_eltNROWS <- elementNROWS(x)
    if (!is(start, "IntegerRanges"))
        return(solveUserSEW(x_eltNROWS, start=start, end=end, width=width))
    if (!(identical(end, NA) && identical(width, NA)))
        stop(wmsg("'end' or 'width' should not be specified or must be ",
                  "set to NA when 'start' is an IntegerRanges object"))
    if (!is(start, "IRanges"))
        start <- as(start, "IRanges")
    ir <- S4Vectors:::V_recycle(start, x, x_what="start", skeleton_what="x")
    if (any(start(ir) < 1L) || any(end(ir) > x_eltNROWS))
        stop(wmsg("'start' contains out-of-bounds ranges"))
    ir
}

setMethod("windows", "list_OR_List",
    function(x, start=NA, end=NA, width=NA)
    {
        ir <- make_IRanges_from_windows_args(x, start, end, width)
        if (length(x) == 0L)
            return(x)

        ## -- Slow path (loops over the list elements of 'x') --

        #for (k in seq_along(x))
        #    x[[k]] <- extractROWS(x[[k]], ir[k])
        #return(x)

        ## -- Fast path --

        ## Unlist 'x' (preserving the inner names) and shift the ranges
        ## in 'ir'.
        if (is.list(x)) {
            unlisted_x <- bindROWS(x[[1L]], x[-1L])
        } else {
            unlisted_x <- unlist(x, use.names=FALSE)
        }
        offsets <- c(0L, end(PartitioningByEnd(x))[-length(x)])
        ir <- shift(ir, shift=offsets)

        ## Subset.
        unlisted_ans <- extractROWS(unlisted_x, ir)

        ## Relist.
        ans_breakpoints <- cumsum(width(ir))
        ans_partitioning <- PartitioningByEnd(ans_breakpoints, names=names(x))
        ans <- as(relist(unlisted_ans, ans_partitioning), class(x))

        ## Propagate 'metadata(x)' and 'mcols(x)'.
        if (is(x, "List")) {
            metadata(ans) <- metadata(x)
            mcols(ans) <- mcols(x)
        }
        ans
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### narrow()
###
### A recursive version of windows() i.e. on a list-like object it's
### equivalent to:
###
###     mendoapply(narrow, x, start, end, width,
###                        MoreArgs=list(use.names=use.names))
###

setGeneric("narrow", signature="x",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
        standardGeneric("narrow")
)

### Should operate recursively on an ordinary list or an IntegerRangesList,
### GenomicRangesList, DNAStrinSetList, or GAlignmentsList derivative.
### But not on an IRanges, GRanges, DNAStringSet, or GAlignments object
### where it's equivalent to windows().
setMethod("narrow", "ANY",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
    {
        call_windows <- is(x, "Ranges") ||
                        !is(x, "list_OR_List") ||
                        !pcompareRecursively(x)
        if (call_windows) {
            ## We've reached a leaf in the recursion tree.
            ans <- windows(x, start=start, end=end, width=width)
            if (!S4Vectors:::normargUseNames(use.names))
                names(ans) <- NULL
            return(ans)
        }

        if (!is(start, "List"))
            start <- as(start, "List")
        start <- S4Vectors:::VH_recycle(start, x, "start", "x")
        if (!is(width, "List"))
            width <- as(width, "List")
        width <- S4Vectors:::VH_recycle(width, x, "width", "x")
        if (!is(end, "List"))
            end <- as(end, "List")
        end <- S4Vectors:::VH_recycle(end, x, "end", "x")

        if (is(x, "CompressedList")) {
            unlisted_start <- unlist(start, use.names=FALSE)
            unlisted_end <- unlist(end, use.names=FALSE)
            unlisted_width <- unlist(width, use.names=FALSE)
            new_unlistData <- narrow(x@unlistData, start=unlisted_start,
                                                   end=unlisted_end,
                                                   width=unlisted_width,
                                                   use.names=use.names)
            ans <- BiocGenerics:::replaceSlots(x, unlistData=new_unlistData,
                                                  check=FALSE)
            return(ans)
        }
        mendoapply(narrow, x, start, end, width,
                           MoreArgs=list(use.names=use.names))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### heads() and tails()
###
### These are just convenience wrappers around windows().
###
### They do 'mendoapply(head, x, n)' and 'mendoapply(tail, x, n)',
### respectively, but use a fast implementation.
###

.normarg_n <- function(n, x_eltNROWS)
{
    if (!is.numeric(n))
        stop("'n' must be an integer vector")
    if (!is.integer(n))
        n <- as.integer(n)
    if (any(is.na(n)))
        stop("'n' cannot contain NAs")
    n <- pmin(x_eltNROWS, n)
    neg_idx <- which(n < 0L)
    if (length(neg_idx) != 0L)
        n[neg_idx] <- pmax(n[neg_idx] + x_eltNROWS[neg_idx], 0L)
    n
}

heads <- function(x, n=6L)
{
    x_eltNROWS <- unname(elementNROWS(x))
    n <- .normarg_n(n, x_eltNROWS)
    windows(x, start=1L, width=n)
}

tails <- function(x, n=6L)
{
    x_eltNROWS <- unname(elementNROWS(x))
    n <- .normarg_n(n, x_eltNROWS)
    windows(x, end=x_eltNROWS, width=n)
}

