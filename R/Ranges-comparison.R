### =========================================================================
### Comparison of Ranges objects
### -------------------------------------------------------------------------
###

### Converts Ranges object 'x' into a numeric vector of the same length where
### the elements are in the same order as the elements in 'x'.
.toNumericWithCompatibleOrder <- function(x)
{
    if (length(x) == 0L)
        return(integer(0))
    max_width <- max(width(x))
    if (max_width == 0L)
        return(start(x))
    ## Adding 1.00 instead of 1L will avoid the result of the addition to be
    ## NA when 'max_width' is MAX_INT (i.e. 2^31-1).
    start(x) + width(x)/(max_width+1.00)
}


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
    function(x, incomparables=FALSE, fromLast=FALSE, ...)
    {
        if (!identical(incomparables, FALSE))
            stop("\"duplicated\" method for Ranges objects only accepts 'incomparables=FALSE'")
        duplicated(.toNumericWithCompatibleOrder(x), fromLast=fromLast)
    }
)

### Relies on a "[" method for 'x'.
setMethod("unique", "Ranges",
    function(x, incomparables=FALSE, fromLast=FALSE, ...)
    {
        x[!duplicated(x, incomparables=incomparables, fromLast=fromLast)]
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

### Need to explicitly define this generic otherwise the implicit generic in
### package "base" would dispatch on (na.last, decreasing).
### Note that dispatching on ... is supported starting with R 2.8.0 only.
### WARNING: Here are 2 common pitfalls when implementing an "order" method:
###   - order(x, decreasing=TRUE) is NOT equivalent to rev(order(x));
###   - It should be made "stable" for consistent behavior across platforms
###     and consistency with base::order(). Note that C qsort() is NOT "stable"
###     so "order" methods that use qsort() at the C-level need to ultimately
###     break ties by position (this is generally done by adding a little
###     extra code at the end of the comparison function used in the calls to
###     qsort()).
setGeneric("order", signature="...",
    function(..., na.last=TRUE, decreasing=FALSE) standardGeneric("order")
)

### 'orderTwoIntegers(start(x), width(x))' is equivalent to
### 'order(start(x), width(x))' but much faster:
###   library(IRanges)
###   N <- 20000000L  # nb of ranges
###   W <- 40L        # average width of the ranges
###   start <- 1L
###   end <- 55000000L
###   set.seed(777)
###   x_start <- sample(end - W - 2L, N, replace=TRUE)
###   x_width <- W + sample(-3:3, N, replace=TRUE)
###   x <- IRanges(start=x_start, width=x_width)
###   ## Takes < 10 sec.:
###   xo <- IRanges:::orderTwoIntegers(start(x), width(x), FALSE)
###   ## Takes about 1 min.:
###   xo2 <- order(start(x), width(x))
###   identical(xo, xo2)  # TRUE
### 'orderTwoIntegers(start(x), width(x))' is also equivalent to
### 'order(.toNumericWithCompatibleOrder(x))' but still faster:
###   ## Takes about 30 sec.:
###   xo3 <- order(IRanges:::.toNumericWithCompatibleOrder(x))
###   identical(xo, xo3)  # TRUE
setMethod("order", "Ranges",
    function(..., na.last=TRUE, decreasing=FALSE)
    {
        if (!isTRUEorFALSE(decreasing))
            stop("'decreasing' must be TRUE or FALSE")
        ## all arguments in '...' are guaranteed to be Ranges objects
        args <- list(...)
        if (length(args) == 1) {
            x <- args[[1L]]
            return(orderTwoIntegers(start(x), width(x),
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

