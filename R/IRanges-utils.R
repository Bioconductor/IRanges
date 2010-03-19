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
        width <- as.integer(width)  # this drops the names
    else if (!is.null(names(width)))
        names(width) <- NULL  # unname() is broken on vector of length 0
    if (anyMissingOrOutside(width, 0L))
        stop("'width' cannot contain NAs or negative values")
    if (!is.numeric(gapwidth))
        stop("'gapwidth' must be an integer vector")
    if (!is.integer(gapwidth))
        gapwidth <- as.integer(gapwidth)
    if (anyMissing(gapwidth))
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
    ans_start <- cumsum(width[-length(width)] + gapwidth)
    ans_start <- from + c(0L, ans_start)
    new2("IRanges", start=ans_start, width=width, check=FALSE)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "centeredIRanges" function.
###

centeredIRanges <- function(center, flank)
{
    if (!is.numeric(center))
        stop("'center' must be a numeric vector")
    if (!is.numeric(flank))
        stop("'flank' must be a numeric vector")
    IRanges(start=center-flank, end=center+flank)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "whichAsIRanges" function.
###
### Note that unlike the standard which() function, whichAsIRanges() drops
### the names of 'x'.
###

whichAsIRanges <- function(x)
{
    if (!is.logical(x))
        stop("'x' must be a logical vector")
    as(x, "NormalIRanges")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercing an IRanges object to a NormalIRanges object.
###

asNormalIRanges <- function(x, force=TRUE)
{
    if (!is(x, "Ranges"))
        stop("'x' must be an Ranges object")
    else if (!is(x, "IRanges"))
        x <- as(x, "IRanges")
    if (!isTRUEorFALSE(force))
        stop("'force' must be TRUE or FALSE")
    if (force)
        x <- reduce(x, drop.empty.ranges=TRUE)
    newNormalIRangesFromIRanges(x, check=!force)
}

.asNormalIRanges <- function(from) asNormalIRanges(from, force=TRUE)

setAs("IRanges", "NormalIRanges", .asNormalIRanges)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### NormalIRanges Inter-interval endomorphisms.
###

setMethod("flank", "NormalIRanges",
    function(x, width, start = TRUE, both = FALSE, use.names = TRUE)
        stop("flanking a ", class(x), " instance is not supported")
)

setMethod("narrow", "NormalIRanges",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
        stop("narrowing a ", class(x), " instance is not supported")
)

setMethod("reflect", "NormalIRanges",
    function(x, bounds, use.names = TRUE)
        stop("reflecting a ", class(x), " instance is not supported")
)

setMethod("resize", "NormalIRanges",
    function(x, width, fix="start", use.names=TRUE, ...)
        stop("resizing a ", class(x), " instance is not supported")
)

setMethod("restrict", "NormalIRanges",
    function(x, start=NA, end=NA, keep.all.ranges=FALSE, use.names=TRUE)
    {
        start <- normargSingleStartOrNA(start)
        end <- normargSingleEndOrNA(end)
        if (!identical(keep.all.ranges, FALSE))
            stop("'keep.all.ranges' argument not supported")
        use.names <- normargUseNames(use.names)

        ans_start <- start(x)
        ans_end <- end(x)
        if (use.names) ans_names <- names(x) else ans_names <- NULL

        if (!is.na(start)) {
            far_too_left <- ans_end < start
            keep_it <- !far_too_left
            ans_start <- ans_start[keep_it]
            ans_end <- ans_end[keep_it]
            if (!is.null(ans_names))
                ans_names <- ans_names[keep_it]
            ## "fix" ans_start
            too_left <- ans_start < start
            ans_start[too_left] <- start
        }
        if (!is.na(end)) {
            far_too_right <- ans_start > end
            keep_it <- !far_too_right
            ans_start <- ans_start[keep_it]
            ans_end <- ans_end[keep_it]
            if (!is.null(ans_names))
                ans_names <- ans_names[keep_it]
            ## "fix" ans_end
            too_right <- end < ans_end
            ans_end[too_right] <- end
        }
        ans_width <- ans_end - ans_start + 1L

        unsafe.update(x, start=ans_start, width=ans_width, names=ans_names)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Intra-interval endomorphisms.
###
### These are optimized at the C-level.
###

setMethod("range", "IRanges",
    function(x, ..., na.rm)
    {
        args <- unname(list(x, ...))
        if (!all(sapply(args, is, "IRanges")))
            stop("all arguments in '...' must be IRanges objects")
        x <- do.call(c, args)
        .Call("IRanges_range", x, PACKAGE="IRanges")
    }
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
        C_ans <- .Call("IRanges_reduce",
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "threebands" method.
###

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
