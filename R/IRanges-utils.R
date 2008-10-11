### =========================================================================
### Utility functions for creating or modifying IRanges objects
### -------------------------------------------------------------------------
###

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "successiveIRanges" function.
###
### Note that the returned IRanges object is guaranteed to be normal in the
### following cases:
###   (a) when length(width) == 0
###   (b) when length(width) == 1 and width > 0
###   (c) when length(width) >= 2 and all(width > 0) and all(gapwidth > 0)
### However, the function doesn't try to turn the result into a NormalIRanges
### object.
###

successiveIRanges <- function(width, gapwidth=0, from=1)
{
    if (!is.numeric(width))
        stop("'width' must be an integer vector")
    if (length(width) == 0)
        return(IRanges())
    if (!is.integer(width))
        width <- as.integer(width)
    if (any(is.na(width)))
        stop("'width' cannot contain NAs")
    if (min(width) < 0L)
        stop("'width' cannot contain negative values")
    if (!is.numeric(gapwidth))
        stop("'gapwidth' must be an integer vector")
    if (!is.integer(gapwidth))
        gapwidth <- as.integer(gapwidth)
    if (any(is.na(gapwidth)))
        stop("'gapwidth' cannot contain NAs")
    if (length(gapwidth) != length(width) - 1) {
        if (length(gapwidth) != 1)
            stop("'gapwidth' must a single integer or an integer vector ",
                 "with one less element than the 'width' vector")
        gapwidth <- rep.int(gapwidth, length(width) - 1)
    }
    if (!isSingleNumber(from))
        stop("'from' must be a single integer")
    if (!is.integer(from))
        from <- as.integer(from)
    ## diffinv() does not preserve the "integer" storage mode!
    ans_start <- as.integer(diffinv(width))
    ans_start <- from + ans_start[-length(ans_start)] + as.integer(diffinv(gapwidth))
    new2("IRanges", start=ans_start, width=width, check=FALSE)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "whichAsIRanges" and "ncharAsIRanges" functions.
###
### Note that unlike the standard which() and nchar() functions, the 2
### functions below drop the names of 'x'.
###

whichAsIRanges <- function(x)
{
    if (!is.logical(x))
        stop("'x' must be a logical vector")
    .Call("which_as_IRanges", x, PACKAGE="IRanges")
}

ncharAsIRanges <- function(x)
{
    if (!is.character(x))
        stop("'x' must be a character vector")
    new2("IRanges", start=rep.int(1L, length(x)),
                    width=nchar(x, type="bytes"), check=FALSE)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "shift" generic and methods.
###
### Shifting preserves normality.
###

setGeneric("shift", signature="x",
    function(x, shift, use.names=TRUE) standardGeneric("shift")
)

setMethod("shift", "IRanges",
    function(x, shift, use.names=TRUE)
    {
        if (!isSingleNumber(shift))
            stop("'shift' must be a single integer")
        if (!is.integer(shift))
            shift <- as.integer(shift)
        if (!normargUseNames(use.names))
            names(x) <- NULL
        unsafe.start(x) <- start(x) + shift
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "restrict" generic and methods.
###
### Note that when used with 'keep.all.ranges=FALSE', restrict() preserves
### normality.
###

setGeneric("restrict", signature="x",
    function(x, start=NA, end=NA, keep.all.ranges=FALSE, use.names=TRUE)
        standardGeneric("restrict")
)

setMethod("restrict", "IRanges",
    function(x, start=NA, end=NA, keep.all.ranges=FALSE, use.names=TRUE)
    {
        start <- normargSingleStartOrNA(start)
        end <- normargSingleEndOrNA(end)
        if (!isTRUEorFALSE(keep.all.ranges))
            stop("'keep.all.ranges' must be TRUE or FALSE")
        use.names <- normargUseNames(use.names)

        ans_start <- start(x)
        ans_end <- end(x)
        if (use.names) ans_names <- names(x) else ans_names <- NULL

        if (!is.na(start)) {
            far_too_left <- ans_end < start
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
            far_too_right <- end < ans_start
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

        unsafe.update(x, start=ans_start, width=ans_width, names=ans_names)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "narrow" generic and methods.
###
### Note that in general, narrow() does NOT preserve normality.
###

setGeneric("narrow", signature="x",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
        standardGeneric("narrow")
)

setMethod("narrow", "IRanges",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
    {
        solved_SEW <- solveUserSEW(width(x), start=start, end=end, width=width)
        unsafe.width(x) <- width(solved_SEW)
        unsafe.start(x) <- start(x) + start(solved_SEW) - 1L
        if (!normargUseNames(use.names))
            names(x) <- NULL
        x
    }
)

setMethod("narrow", "NormalIRanges",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
        stop("narrowing a ", class(x), " instance is not supported")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "reduce" methods.
###
### Note that reduce() preserves normality (of course).
###

setMethod("reduce", "IRanges",
    function(x, with.inframe.attrib=FALSE)
    {
        if (!isTRUEorFALSE(with.inframe.attrib))
            stop("'with.inframe.attrib' must be TRUE or FALSE")
        C_ans <- .Call("IRanges_reduce",
                        x, with.inframe.attrib,
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "gaps" method.
###
### Note that gaps() will always return a normal IRanges object (so, obviously,
### it preserves normality).
###

setMethod("gaps", "IRanges",
    function(x, start=NA, end=NA)
    {
        start <- normargSingleStartOrNA(start)
        end <- normargSingleEndOrNA(end)
        ## No matter in what order restricting and normalizing are done, the final
        ## result should always be exactly the same.
        ## Now which order is the most efficient? It depends...
        xx <- asNormalIRanges(x, force=TRUE)
        xx0 <- restrict(xx, start=start, end=end) # preserves normality
        ans_start <- ans_width <- integer(0)
        if (isEmpty(xx0)) {
            if (is.na(start) || is.na(end))
                stop("'x' is not overlapping with the unbounded region ",
                     "represented by 'start' and 'end'")
            if (start <= end) {
                ans_start <- start
                ans_width <- end - start + 1L
            }
        } else {
            start0 <- start(xx0)
            end0 <- end(xx0)
            if (!is.na(start) && start < min(xx0)) {
                start0 <- c(start, start0)
                end0 <- c(start - 1L, end0)
            }
            if (!is.na(end) && max(xx0) < end) {
                start0 <- c(start0, end + 1L)
                end0 <- c(end0, end)
            }
            if (length(start0) >= 2) {
                ans_start <- end0[-length(start0)] + 1L
                ans_width <- start0[-1] - ans_start
            } 
        }
        unsafe.update(x, start=ans_start, width=ans_width, names=NULL)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Overlap tools
###

## find objects in the index that overlap those in a query set
setGeneric("overlap", function(object, query, ...) standardGeneric("overlap"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercing an IRanges object to a NormalIRanges object.
###

asNormalIRanges <- function(x, force=TRUE)
{
    if (!is(x, "IRanges"))
        stop("'x' must be an IRanges object")
    if (!isTRUEorFALSE(force))
        stop("'force' must be TRUE or FALSE")
    if (!force)
        return(newNormalIRangesFromIRanges(x, check=TRUE))
    x1 <- as(x, "IRanges") # downgrade
    x2 <- reduce(x1)
    x3 <- x2[width(x2) != 0]
    newNormalIRangesFromIRanges(x3, check=FALSE)
}

.asNormalIRanges <- function(from) asNormalIRanges(from, force=TRUE)

setAs("IRanges", "NormalIRanges", .asNormalIRanges)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Deprecated stuff.
###

intToRanges <- function(x)
{
    msg <- paste("'intToRanges' is deprecated.",
                 "Use 'IRanges(start=rep.int(1L, length(x)), width=x)' instead.",
                 sep="\n")
    .Deprecated(msg=msg)
    if (!is.numeric(x))
        stop("'x' must be an integer vector")
    if (!is.integer(x))
        x <- as.integer(x)
    if (min(x) < 0L)
        stop("'x' cannot contain negative integers")
    new2("IRanges", start=rep.int(1L, length(x)), width=x, check=FALSE)
}

intToAdjacentRanges <- function(...)
{
    .Deprecated("successiveIRanges")
    successiveIRanges(...)
}

toNormalIRanges <- function(x)
{
    .Deprecated("asNormalIRanges")
    asNormalIRanges(x, force=TRUE)
}

