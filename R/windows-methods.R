### =========================================================================
### windows()
### -------------------------------------------------------------------------


### windows() is a "parallel" version of window() for list-like objects. That
### is, it does 'mendoapply(window, x, start, end, width)' but uses a fast
### implementation.
setGeneric("windows", signature="x",
    function(x, start=NA, end=NA, width=NA) standardGeneric("windows")
)

### Not exported.
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

        ## Unlist 'x' and shift the ranges in 'ir'.
        if (is.list(x))
            unlisted_x <- unlist(x, recursive=FALSE, use.names=FALSE)
        else
            unlisted_x <- unlist(x, use.names=FALSE)
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

setGeneric("narrow", signature="x",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
        standardGeneric("narrow")
)

### NOT exported but used in Biostrings package.
### By default, narrow() is equivalent to windows(). Must be overwritten by
### IntegerRangesList or GenomicRangesList derivatives and GAlignmentsList
### objects.
default_narrow <- function(x, start=NA, end=NA, width=NA, use.names=TRUE)
{
    x <- windows(x, start=start, end=end, width=width)
    if (!S4Vectors:::normargUseNames(use.names))
        names(x) <- NULL
    x
}

setMethod("narrow", "Vector", default_narrow)


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

