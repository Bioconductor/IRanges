### =========================================================================
### Comparison of Ranges objects
### -------------------------------------------------------------------------
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### compare()
###

setGeneric("compare", function(x, y) standardGeneric("compare"))

### Generalized range-wise comparison of 2 Ranges objects.
###   > x <- IRanges(1:11, width=4)
###   > y <- IRanges(6, 9)
###   > compare(x, y)
###    [1] -6 -5 -4 -4 -4  0  4  4  4  5  6
###   > compare(IRanges(4:6, width=6), y)
###   [1] -3 -2  1
###   > compare(IRanges(6:8, width=2), y)
###   [1] -1  2  3
###   > compare(x, y) < 0  # equivalent to x < y
###   > compare(x, y) == 0  # equivalent to x == y
###   > compare(x, y) > 0  # equivalent to x > y
### TODO: Seems like using compare() to implement "==", "!=", "<=", ">=",
### "<" and ">" methods for Ranges objects would make them slightly faster
### (between 1.5x and 2.5x) and also slightly more memory efficient.
setMethod("compare", c("Ranges", "Ranges"),
    function(x, y)
    {
        .Call2("Ranges_compare",
               start(x), width(x), start(y), width(y),
               PACKAGE="IRanges")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Equality and related methods.
###

setMethod("==", signature(e1="Ranges", e2="Ranges"),
    function(e1, e2)
    {
        (start(e1) == start(e2)) & (width(e1) == width(e2))
    }
)

setMethod("!=", signature(e1="Ranges", e2="Ranges"),
    function(e1, e2)
    {
        ## Should be slightly more efficient than '!(e1 == e2)', at least in
        ## theory.
        (start(e1) != start(e2)) | (width(e1) != width(e2))
    }
)

setMethod("duplicated", "Ranges",
    function(x, incomparables=FALSE, fromLast=FALSE,
             method=c("auto", "quick", "hash"), ...)
    {
        if (!identical(incomparables, FALSE))
            stop("\"duplicated\" method for Ranges objects ",
                 "only accepts 'incomparables=FALSE'")
        duplicatedIntegerPairs(start(x), width(x),
                               fromLast=fromLast, method=method)
    }
)

### Relies on a "[" method for 'x'.
setMethod("unique", "Ranges",
    function(x, incomparables=FALSE, fromLast=FALSE,
             method=c("auto", "quick", "hash"), ...)
    {
        x[!duplicated(x, incomparables=incomparables,
                         fromLast=fromLast, method=method, ...)]
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Ordering and related methods.
###
### Ranges are ordered by starting position first and then by width.
### This way, the space of ranges is totally ordered.
### The "order", "sort" and "rank" methods for Ranges objects are consistent
### with this order.
###

setMethod("<=", signature(e1="Ranges", e2="Ranges"),
    function(e1, e2)
    {
         (start(e1) < start(e2)) | ((start(e1) == start(e2)) & (width(e1) <= width(e2)))
    }
)

setMethod(">=", signature(e1="Ranges", e2="Ranges"),
    function(e1, e2) {e2 <= e1}
)

setMethod("<", signature(e1="Ranges", e2="Ranges"),
    function(e1, e2)
    {
         (start(e1) < start(e2)) | ((start(e1) == start(e2)) & (width(e1) < width(e2)))
    }
)

setMethod(">", signature(e1="Ranges", e2="Ranges"),
    function(e1, e2) {e2 < e1}
)

setMethod("order", "Ranges",
    function(..., na.last=TRUE, decreasing=FALSE)
    {
        if (!isTRUEorFALSE(decreasing))
            stop("'decreasing' must be TRUE or FALSE")
        ## all arguments in '...' are guaranteed to be Ranges objects
        args <- list(...)
        if (length(args) == 1) {
            x <- args[[1L]]
            return(orderIntegerPairs(start(x), width(x),
                                     decreasing = decreasing))
        }
        order_args <- vector("list", 2L*length(args))
        order_args[2L*seq_len(length(args)) - 1L] <- lapply(args, start)
        order_args[2L*seq_len(length(args))] <- lapply(args, end)
        do.call(order, c(order_args, na.last=na.last, decreasing=decreasing))
    }
)

### Relies on a "[" method for 'x'.
setMethod("sort", "Ranges",
    function(x, decreasing=FALSE, ...) 
    {
        x[order(x, decreasing=decreasing)]
    }
)

setMethod("rank", "Ranges",
    function(x, na.last=TRUE, ties.method=c("average", "first", "random", "max", "min"))
    {
        if (!missing(ties.method) && !identical(ties.method, "first"))
            stop("only 'ties.method=\"first\"' is supported when ranking ranges")
        xo <- order(x)
        ## 'ans' is the reverse permutation of 'xo'
        ans <- integer(length(xo))
        ans[xo] <- seq_len(length(xo))
        ans
    }
)

