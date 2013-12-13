### =========================================================================
### Group elements of a vector-like object into a list-like object
### -------------------------------------------------------------------------
###
### What should go in this file?
###
### - splitAsListReturnedClass() generic and default method.
### - All "relist" and "split" methods defined in IRanges should be here.
### - extractList() generic and default method.
###
### TODO: Maybe put the default methods for the reverse transformations here
### (unlist, unsplit, and unsplit<-).
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### splitAsListReturnedClass()
###

setGeneric("splitAsListReturnedClass",
    function(x) standardGeneric("splitAsListReturnedClass")
)

setMethod("splitAsListReturnedClass", "ANY",
    function(x) {
      cn <- listClassName("Compressed", class(x))
      if (cn == "CompressedList")
        cn <- listClassName("Simple", class(x))
      cn
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### relist()
###

setMethod("relist", c("ANY", "PartitioningByEnd"),
    function(flesh, skeleton)
    {
        ans_class <- splitAsListReturnedClass(flesh)
        skeleton_len <- length(skeleton)
        if (skeleton_len == 0L) {
            flesh_len2 <- 0L
        } else {
            flesh_len2 <- end(skeleton)[skeleton_len]
        }
        if (NROW(flesh) != flesh_len2)
            stop("shape of 'skeleton' is not compatible with 'NROW(flesh)'")
        if (extends(ans_class, "CompressedList"))
            return(newCompressedList0(ans_class, flesh, skeleton))
        if (!extends(ans_class, "SimpleList"))
            stop("don't know how to split or relist a ", class(flesh),
                 " object as a ", ans_class, " object")
        listData <- lapply(skeleton, function(i) extractROWS(flesh, i))

        ## TODO: Once "window" methods have been revisited/tested and
        ## 'window(flesh, start=start, end=end)' is guaranteed to do the
        ## right thing for any 'flesh' object (in particular it subsets a
        ## data.frame-like object along the rows), then replace the line above
        ## by the code below (which should be more efficient):

        #skeleton_start <- start(skeleton)
        #skeleton_end <- end(skeleton)
        #FUN <- function(start, end) window(flesh, start=start, end=end)
        #names(skeleton_start) <- names(skeleton)
        #listData <- mapply(FUN, skeleton_start, skeleton_end)

        ## or, if we don't trust mapply():

        #skeleton_start <- start(skeleton)
        #skeleton_end <- end(skeleton)
        #X <- seq_len(skeleton_len)
        #names(X) <- names(skeleton)
        #listData <- lapply(X, function(i) window(flesh,
        #                                         start=skeleton_start[i],
        #                                         end=skeleton_end[i]))

        newList(ans_class, listData)
    }
)

setMethod("relist", c("ANY", "List"),
    function(flesh, skeleton)
    {
        relist(flesh, PartitioningByEnd(skeleton))
    }
)

setMethod("relist", c("Vector", "list"),
    function(flesh, skeleton)
    {
        relist(flesh, PartitioningByEnd(skeleton))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### splitAsList() and split()
###

.splitAsList_by_integer <- function(x, f, drop)
{
    if (length(f) > NROW(x))
        stop("'f' cannot be longer than 'NROW(x)' when it's an integer vector")
    idx <- orderInteger(f)
    tmp <- Rle(f[idx])
    f <- cumsum(runLength(tmp))
    names(f) <- as.character(runValue(tmp))
    if (!identical(drop, FALSE))
        warning("'drop' is ignored when 'f' is an integer vector")
    x <- extractROWS(x, idx)
    f <- PartitioningByEnd(f)
    relist(x, f)
}

.splitAsList_by_factor <- function(x, f, drop)
{
    x_NROW <- NROW(x)
    f_len <- length(f)
    f_levels <- levels(f)
    f <- as.integer(f)
    if (f_len > x_NROW)
        f <- head(f, n=x_NROW)
    idx <- orderInteger(f)
    f <- tabulate(f, nbins=length(f_levels))
    names(f) <- f_levels
    if (drop)
        f <- f[f != 0L]
    f <- cumsum(f)
    x <- extractROWS(x, idx)
    f <- PartitioningByEnd(f)
    relist(x, f)
}

.splitAsList_by_integer_Rle <- function(x, f, drop)
{
    if (length(f) > NROW(x))
        stop("'f' cannot be longer than data when it's an integer-Rle")
    f_vals <- runValue(f)
    f_lens <- runLength(f)
    idx <- orderInteger(f_vals)
    xranges <- successiveIRanges(f_lens)[idx]
    tmp <- Rle(f_vals[idx], f_lens[idx])
    f <- cumsum(runLength(tmp))
    names(f) <- as.character(runValue(tmp))
    if (!identical(drop, FALSE))
        warning("'drop' is ignored when 'f' is an integer-Rle")
    x <- extractROWS(x, xranges)
    f <- PartitioningByEnd(f)
    relist(x, f)
}

.splitAsList_by_Rle <- function(x, f, drop)
{
    x_NROW <- NROW(x)
    f_len <- length(f)
    f_vals <- runValue(f)
    if (!is.factor(f_vals)) {
        f_vals <- as.factor(f_vals)
        if (f_len > x_NROW) {
            runValue(f) <- f_vals
            f <- head(f, n=x_NROW)
            f_vals <- runValue(f)
        }
    } else if (f_len > x_NROW) {
        f <- head(f, n=x_NROW)
        f_vals <- runValue(f)
    }
    f_lens <- runLength(f)
    f_levels <- levels(f_vals)
    f_vals <- as.integer(f_vals)
    idx <- orderInteger(f_vals)
    xranges <- successiveIRanges(f_lens)[idx]
    ## Using tabulate2() is 5x faster than doing:
    ##   f <- integer(length(f_levels))
    ##   tmp <- Rle(f_vals[idx], f_lens[idx])
    ##   f[runValue(tmp)] <- runLength(tmp)
    f <- tabulate2(f_vals, nbins=length(f_levels), weight=f_lens)
    names(f) <- f_levels
    if (drop)
        f <- f[f != 0L]
    f <- cumsum(f)
    x <- extractROWS(x, xranges)
    f <- PartitioningByEnd(f)
    relist(x, f)
}

splitAsList <- function(x, f, drop=FALSE)
{
    if (!isTRUEorFALSE(drop))
        stop("'drop' must be TRUE or FALSE")
    if (!((is.vector(f) && is.atomic(f)) || is.factor(f) || is(f, "Rle")))
        stop("'f' must be an atomic vector or a factor (possibly in Rle form)")
    x_NROW <- NROW(x)
    f_len <- length(f)
    if (f_len < x_NROW) {
        if (f_len == 0L)
            stop("'length(f)' is 0 but 'NROW(x)' is > 0")
        if (x_NROW %% f_len != 0L)
            warning("'NROW(x)' is not a multiple of 'length(f)'")
        f <- rep(f, length.out=x_NROW)
    }
    if (is.integer(f))
        return(.splitAsList_by_integer(x, f, drop))
    if (is.vector(f) && is.atomic(f))
        f <- as.factor(f)
    if (is.factor(f))
        return(.splitAsList_by_factor(x, f, drop))
    ## From now on, 'f' is guaranteed to be an Rle.
    f_vals <- runValue(f)
    if (!((is.vector(f_vals) && is.atomic(f_vals)) || is.factor(f_vals)))
        stop("'f' must be an atomic vector or a factor (possibly in Rle form)")
    if (is.integer(f_vals))
        return(.splitAsList_by_integer_Rle(x, f, drop))
    return(.splitAsList_by_Rle(x, f, drop))
}

setMethod("split", c("Vector", "ANY"),
    function(x, f, drop=FALSE) splitAsList(x, f, drop=drop)
)

setMethod("split", c("ANY", "Vector"),
    function(x, f, drop=FALSE) splitAsList(x, f, drop=drop)
)

setMethod("split", c("Vector", "Vector"),
    function(x, f, drop=FALSE) splitAsList(x, f, drop=drop)
)

setMethod("split", c("list", "Vector"),
    function(x, f, drop=FALSE, ...) split(x, as.vector(f), drop=drop, ...)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### extractList()
###
### Would extractGroups be a better name for this?
### Or extractGroupedROWS? (analog to extractROWS, except that the ROWS are
### grouped).
###

### 'x' must be a vector-like object and 'i' a list-like object.
### Must return a list-like object parallel to 'i' and with same "shape" as
### 'i' (i.e. same elementLengths). If 'i' has names, they should be
### propagated to the returned value. The list elements of the returned value
### must have the class of 'x'.
setGeneric("extractList", function(x, i) standardGeneric("extractList"))

### Default method.
setMethod("extractList", c("ANY", "ANY"),
    function(x, i)
    {
        if (is(i, "Ranges"))
            return(relist(extractROWS(x, i), i))
        if (is.list(i)) {
            unlisted_i <- unlist(i, recursive=FALSE, use.names=FALSE)
        } else if (is(i, "List")) {
            ## The various "unlist" methods for List derivatives don't know
            ## how to operate recursively and don't support the 'recursive'
            ## arg.
            unlisted_i <- unlist(i, use.names=FALSE)
        } else {
            stop("'i' must be a list-like object")
        }
        relist(extractROWS(x, unlisted_i), i)
    }
)

