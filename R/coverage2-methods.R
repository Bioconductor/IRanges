### =========================================================================
### coverage2()
### -------------------------------------------------------------------------
###
### A better coverage().
###
### It all started when I came across this:
### https://stackoverflow.com/questions/17138760/counting-overlaps-of-integer-ranges
###

setGeneric("coverage2", signature="x",
    function(x, from=NA, to=NA, weight=1L, varname="cvg", collapse=FALSE, ...)
        standardGeneric("coverage2")
)

### Methods for IntegerRanges and IntegerRangesList (defined in this file)
### add the 'circle.length' argument.
### Method for GenomicRanges objects adds the 'ignore.strand' argument.


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Restriction window helpers
###
### Use these helpers in the "restrict" and "gaps" methods.
###
### Should we turn these helpers into methods of a generic function?
### Like effectiveRestrictionWindow()? or effectiveFromTo()?
### Also maybe export and document them so the user can actually use them
### to see what effective restriction windows are being used and also see
### the invalid windows causing an error (by calling
### effectiveRestrictionWindow() with check=FALSE).
###

### Return an integer vector of length 2.
effective_restriction_window_for_IntegerRanges <-
    function(x, from=NA, to=NA, check=TRUE)
{
    stopifnot(is(x, "IntegerRanges"),
              isSingleNumberOrNA(from),
              isSingleNumberOrNA(to))
    if (!is.integer(from))
        from <- as.integer(from)
    if (!is.integer(to))
        to <- as.integer(to)
    if (is.na(from) || is.na(to)) {
        if (length(x) == 0L)
            return(c(from=from, to=to))
        x_range <- range(x)
        if (is.na(from))
            from <- start(x_range)
        if (is.na(to))
            to <- end(x_range)
    }
    if (check) {
        width <- to - from + 1L
        if (width < 0L)
            stop(wmsg("invalid from-to: ", from, "-", to))
    }
    c(from=from, to=to)
}

### Return an N x 2 integer matrix where N is length(x).
effective_restriction_windows_for_IntegerRangesList <-
    function(x, from=NA, to=NA, check=TRUE)
{
    stopifnot(is(x, "IntegerRangesList"),
              is.numeric(from) || is.logical(from) && all(is.na(from)),
              is.numeric(to) || is.logical(to) && all(is.na(to)))
    if (!is.integer(from))
        from <- as.integer(from)
    if (!is.integer(to))
        to <- as.integer(to)
    from <- S4Vectors:::V_recycle(from, x, "from", "x")
    to <- S4Vectors:::V_recycle(to, x, "to", "x")
    x_range <- range(x)
    na_idx <- which(is.na(from))
    from[na_idx] <- as.integer(start(x_range))[na_idx]
    na_idx <- which(is.na(to))
    to[na_idx] <- as.integer(end(x_range))[na_idx]
    if (check) {
        width <- to - from + 1L
        if (any(width < 0L, na.rm=TRUE))
            stop(wmsg("some of the restriction windows defined by the ",
                      "supplied 'from' and 'to' have a negative width"))
    }
    cbind(from=from, to=to)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### "coverage2" methods
###

### TODO: Add and support the 'circle.length' argument to the 2 methods
### below.

### Takes an IntegerRanges derivative 'x' and returns its coverage as the
### metadata column of an IPos object by default. If 'collapse' is TRUE',
### the coverage is returned in an IRanges object instead.
### The 'from' and 'to' arguments control the window of integer positions
### for which to compute and return coverage.
### Each of these arguments must be a single integer or NA. When set to NA
### (the default), 'from' is replaced internally with 'min(start(x))',
### and 'to' with 'max(end(x))'.
### All the integer positions in the from-to window are represented in the
### returned object. More precisely, the returned IPos or IRanges 'ans' is
### disjoint, strictly sorted, and with no gaps between the ranges, and its
### ranges span the from-to window (i.e. 'reduce(ans)' will return the single
### range from-to). In particular, when 'ans' is an IPos object, 'pos(ans)'
### returns the from:to sequence.
coverage2_IntegerRanges <- function(x, from=NA, to=NA, weight=1L,
                                       varname="cvg", collapse=FALSE)
{
    stopifnot(isSingleString(varname), isTRUEorFALSE(collapse))
    from_to <- effective_restriction_window_for_IntegerRanges(x, from, to)
    shift <- 1L - from_to[[1L]]
    width <- from_to[[2L]] + shift
    if (length(x) == 0L) {
        if (is.na(width) || width == 0L) {
            ans <- IRanges()
        } else {
            ans <- IRanges(from_to[[1L]], width=width)
        }
        ## 'weight' determines the type of Rle.
        cvg <- Rle(weight * 0L, sum(width(ans)))
    } else {
        cvg <- coverage(x, shift=shift, width=width, weight=weight)  # Rle
        ans_width <- runLength(cvg)
        ans_end <- cumsum(ans_width) - shift
        ans <- IRanges(end=ans_end, width=ans_width)
    }
    if (collapse) {
        var <- runValue(cvg)
    } else {
        ans <- IPos(ans)
        var <- cvg
    }
    mcols(ans) <- S4Vectors:::new_DataFrame(setNames(list(var), varname))
    ans
}

### Takes an IntegerRangesList derivative 'x' and returns its coverage as the
### inner metadata column of an IPosList object by default. If 'collapse' is
### TRUE, the coverage is returned in an IRangesList object instead.
### The 'from' and 'to' arguments control the windows of integer positions
### for which to compute and return coverage.
### Each of these arguments must be an integer vector parallel to 'x',
### possibly with NAs. If shorter than 'x', they'll be recycled to the length
### of 'x'.
### The object to return is computed with a fast implementation of
###
###     mapply(coverage2_IntegerRanges, x, from, to, weight,
###            MoreArgs=list(varname=varname, collapse=collapse))
###
### and then returned as an IPosList or IRangesList, obeying 'collapse'.
coverage2_IntegerRangesList <- function(x, from=NA, to=NA, weight=1L,
                                           varname="cvg", collapse=FALSE)
{
    stopifnot(isSingleString(varname), isTRUEorFALSE(collapse))
    from_to <- effective_restriction_windows_for_IntegerRangesList(x, from, to)
    shift <- 1L - unname(from_to[ , 1L])
    width <- unname(from_to[ , 2L]) + shift
    cvg <- coverage(x, shift=shift, width=width, weight=weight)  # SimpleRleList
    ans_width <- as(runLength(cvg), "CompressedIntegerList")
    ans_end <- as(cumsum(ans_width), class(ans_width)) - shift
    unlisted_ans <- IRanges(end=unlist(ans_end, use.names=FALSE),
                            width=unlist(ans_width, use.names=FALSE))
    if (collapse) {
        var <- unlist(runValue(cvg), use.names=FALSE)
    } else {
        unlisted_ans <- IPos(unlisted_ans)
        if (length(cvg) == 0L) {
            ## 'weight' determines the type of Rle.
            var <- Rle(weight * 0L, 0L)
        } else {
            var <- unlist(cvg, use.names=FALSE)
        }
    }
    mcols(unlisted_ans) <- S4Vectors:::new_DataFrame(
                               setNames(list(var), varname))
    if (collapse) {
        ans <- relist(unlisted_ans, ans_width)
    } else {
        ans <- relist(unlisted_ans, cvg)
    }
    ans
}

setMethod("coverage2", "IntegerRanges", coverage2_IntegerRanges)
setMethod("coverage2", "IntegerRangesList", coverage2_IntegerRangesList)

