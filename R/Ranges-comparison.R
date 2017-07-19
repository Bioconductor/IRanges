### =========================================================================
### Comparing and ordering ranges
### -------------------------------------------------------------------------
###


setMethod("pcompareRecursively", "Ranges", function(x) FALSE)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### pcompare()
###
### Ranges are ordered by starting position first and then by width.
### This way, the space of ranges is totally ordered.
### This "pcompare" method returns one of the 13 predefined codes (>= -6 and
### <= 6) described in the man page. The signs of those codes reflect this
### order.
###

setMethod("pcompare", c("Ranges", "Ranges"),
    function(x, y)
    {
        .Call2("Ranges_pcompare",
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
### match()
###

setMethod("match", c("Ranges", "Ranges"),
    function(x, table, nomatch=NA_integer_, incomparables=NULL,
                       method=c("auto", "quick", "hash"))
    {
        if (!is.null(incomparables))
            stop("\"match\" method for Ranges objects ",
                 "only accepts 'incomparables=NULL'")
        ## Equivalent to (but faster than):
        ##     findOverlaps(x, table, type="equal", select="first")
        ## except when 'x' or 'table' contain empty ranges.
        matchIntegerPairs(start(x), width(x),
                          start(table), width(table),
                          nomatch=nomatch, method=method)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### selfmatch()
###

setMethod("selfmatch", "Ranges",
    function(x, method=c("auto", "quick", "hash"))
        selfmatchIntegerPairs(start(x), width(x), method=method)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### order() and related methods.
###
### is.unsorted(), order(), sort(), rank() on Ranges derivatives are
### consistent with the order implied by pcompare().
### is.unsorted() is a quick/cheap way of checking whether a Ranges
### derivative is already sorted, e.g., called prior to a costly sort.
### sort() and rank() will work out-of-the-box on a Ranges derivative thanks
### to the method for List objects (which delegates to the method for Vector
### objects).
###

.Ranges_as_IntegerPairs <- function(x)
{
    a <- start(x)
    b <- width(x)
    list(a, b)
}

setMethod("is.unsorted", "Ranges",
    function(x, na.rm=FALSE, strictly=FALSE)
    {
        if (!identical(na.rm, FALSE))
            warning("\"is.unsorted\" method for Ranges objects ",
                    "ignores the 'na.rm' argument")
        if (!isTRUEorFALSE(strictly))
            stop("'strictly' must be TRUE of FALSE")
        ## It seems that creating the integer pairs below is faster when
        ## 'x' is already sorted (TODO: Investigate why). Therefore, and
        ## somewhat counterintuitively, is.unsorted() can be faster when 'x'
        ## is already sorted (which, in theory, is the worst-case scenario
        ## because S4Vectors:::sortedIntegerPairs() will then need to take a
        ## full walk on 'x') than when it is unsorted (in which case
        ## S4Vectors:::sortedIntegerPairs() might stop walking on 'x' after
        ## checking its first 2 elements only -- the best-case scenario).
        pairs <- .Ranges_as_IntegerPairs(x)
        !S4Vectors:::sortedIntegerPairs(pairs[[1L]], pairs[[2L]],
                                        strictly=strictly)
    }
)

.order_Ranges <- function(x, decreasing=FALSE)
{
    if (!isTRUEorFALSE(decreasing))
        stop("'decreasing' must be TRUE or FALSE")
    pairs <- .Ranges_as_IntegerPairs(x)
    orderIntegerPairs(pairs[[1L]], pairs[[2L]], decreasing=decreasing)
}

### 'na.last' is pointless (Ranges objects don't contain NAs) so is ignored.
### 'method' is also ignored at the moment.
setMethod("order", "Ranges",
    function(..., na.last=TRUE, decreasing=FALSE, method=c("shell", "radix"))
    {
        ## Turn off this warning for now since it triggers spurious warnings
        ## when calling sort() on a RangesList object. The root of the
        ## problem is inconsistent defaults for 'na.last' between order() and
        ## sort(), as reported here:
        ##   https://stat.ethz.ch/pipermail/r-devel/2015-November/072012.html
        #if (!identical(na.last, TRUE))
        #    warning("\"order\" method for Ranges objects ",
        #            "ignores the 'na.last' argument")
        if (!isTRUEorFALSE(decreasing))
            stop("'decreasing' must be TRUE or FALSE")
        ## All arguments in '...' are guaranteed to be Ranges objects.
        args <- list(...)
        if (length(args) == 1L)
            return(.order_Ranges(args[[1L]], decreasing))
        order_args <- c(unlist(lapply(args, .Ranges_as_IntegerPairs),
                               recursive=FALSE, use.names=FALSE),
                        list(na.last=na.last, decreasing=decreasing))
        do.call(order, order_args)
    }
)

