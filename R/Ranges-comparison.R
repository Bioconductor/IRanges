### =========================================================================
### Comparing and ordering ranges
### -------------------------------------------------------------------------
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### compare()
###
### Doing 'compare(x, y)' on 2 vector-like objects 'x' and 'y' of length 1
### must return an integer less than, equal to, or greater than zero if the
### single element in 'x' is considered to be respectively less than, equal
### to, or greater than the single element in 'y'.
###

setGeneric("compare", function(x, y) standardGeneric("compare"))

### Ranges are ordered by starting position first and then by width.
### This way, the space of ranges is totally ordered.
### This "compare" method returns codes that reflect this order.
setMethod("compare", c("Ranges", "Ranges"),
    function(x, y)
    {
        .Call2("Ranges_compare",
               start(x), width(x), start(y), width(y),
               PACKAGE="IRanges")
    }
)

rangeComparisonCodeToLetter <- function(code)
{
    if (!is.integer(code))
        stop("'code' must be an integer vector")
    code <- code + 7L
    code[code < 1L | 14L < code] <- 14L
    levels <- c(letters[1:13], "X")
    structure(code, levels=levels, class="factor")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Element-wise (aka "parallel") comparison of 2 Ranges objects.
###
### We only need to implement "==" and "<=" methods. The other binary
### comparison operations (!=, >=, <, >) will work out-of-the-box on Ranges
### objects thanks to the corresponding methods defined for Vector objects.
###

setMethod("==", signature(e1="Ranges", e2="Ranges"),
    function(e1, e2) { compare(e1, e2) == 0L }
)

setMethod("<=", signature(e1="Ranges", e2="Ranges"),
    function(e1, e2) { compare(e1, e2) <= 0L }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Unique and duplicated elements within a Ranges object.
###

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
### order() and related methods.
###
### The "order", "sort" and "rank" methods for Ranges objects are consistent
### with the order implied by compare().
###

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

