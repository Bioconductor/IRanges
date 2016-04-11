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
### Ordering ranges
###
### order(), sort(), rank() on Ranges objects are consistent with the order
### on ranges implied by pcompare().
###

### 'na.last' is pointless (Ranges objects don't contain NAs) so is ignored.
### 'method' is also ignored at the moment.
setMethod("order", "Ranges",
    function(..., na.last=TRUE, decreasing=FALSE, method=c("shell", "radix"))
    {
        if (!isTRUEorFALSE(decreasing))
            stop("'decreasing' must be TRUE or FALSE")
        ## All arguments in '...' are guaranteed to be Ranges objects.
        args <- list(...)
        if (length(args) == 1L) {
            x <- args[[1L]]
            return(orderIntegerPairs(start(x), width(x),
                                     decreasing=decreasing))
        }
        order_args <- vector("list", 2L * length(args))
        idx <- 2L * seq_along(args)
        order_args[idx - 1L] <- lapply(args, start)
        order_args[idx] <- lapply(args, width)
        do.call(order, c(order_args, list(decreasing=decreasing)))
    }
)

