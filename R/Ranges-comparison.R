### =========================================================================
### Comparing and ordering ranges
### -------------------------------------------------------------------------
###


setMethod("compareRecursively", "Ranges", function(x) FALSE)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### compare()
###
### Ranges are ordered by starting position first and then by width.
### This way, the space of ranges is totally ordered.
### This "compare" method returns one of the 13 predefined codes (>= -6 and
### <= 6) described in the man page. The signs of those codes reflect this
### order.
###

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
### match()
###

setMethod("match", c("Ranges", "Ranges"),
    function(x, table, nomatch=NA_integer_, incomparables=NULL,
                       method=c("auto", "quick", "hash"),
                       match.if.overlap=FALSE)
    {
        if (!isSingleNumberOrNA(nomatch))
            stop("'nomatch' must be a single number or NA")
        if (!is.integer(nomatch))
            nomatch <- as.integer(nomatch)
        if (!is.null(incomparables))
            stop("\"match\" method for Ranges objects ",
                 "only accepts 'incomparables=NULL'")
        if (!isTRUEorFALSE(match.if.overlap))
            stop("'match.if.overlap' must be TRUE or FALSE")
        if (match.if.overlap) {
            msg <- c("  Starting with BioC 2.14, ",
                     "match() on Ranges objects\n  does not support ",
                     "the 'match.if.overlap' argument anymore. Please use\n\n",
                     "    findOverlaps(x, table, select=\"first\")\n\n",
                     "  if you need to do\n\n",
                     "    match(x, table, match.if.overlap=TRUE)")
            .Defunct(msg=msg)
            ans <- findOverlaps(x, table, select="first")
            if (!is.na(nomatch) && anyMissing(ans))
                ans[is.na(ans)] <- nomatch
            return(ans)
        }
        ## Equivalent to (but faster than):
        ##     findOverlaps(x, table, type="equal", select="first")
        ## except when 'x' and 'table' both contain empty ranges.
        matchIntegerPairs(start(x), width(x), start(table), width(table),
                          nomatch=nomatch, method=method)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### selfmatch()
###

### 'match.if.overlap' arg is ignored.
setMethod("selfmatch", "Ranges",
    function(x, method=c("auto", "quick", "hash"), match.if.overlap=FALSE)
    {
        if (!isTRUEorFALSE(match.if.overlap))
            stop("'match.if.overlap' must be TRUE or FALSE")
        if (match.if.overlap) {
            msg <- c("  Starting with BioC 2.14), ",
                     "selfmatch() on a Ranges object\n  does not support ",
                     "the 'match.if.overlap' argument anymore.")
            .Defunct(msg=msg)
        }
        selfmatchIntegerPairs(start(x), width(x), method=method)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### order() and related methods.
###
### The "order" and "rank" methods for Ranges objects are consistent with
### the order implied by compare().
###

setMethod("order", "Ranges",
    function(..., na.last=TRUE, decreasing=FALSE)
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
        order_args <- vector("list", 2L*length(args))
        idx <- 2L*seq_len(length(args))
        order_args[idx - 1L] <- lapply(args, start)
        order_args[idx] <- lapply(args, width)
        do.call(order, c(order_args,
                         list(na.last=na.last, decreasing=decreasing)))
    }
)

setMethod("rank", "Ranges",
    function(x, na.last=TRUE,
             ties.method=c("average", "first", "random", "max", "min"))
    {
        if (!missing(ties.method) && !identical(ties.method, "first"))
            stop("only 'ties.method=\"first\"' is supported ",
                 "when ranking ranges")
        oo <- order(x)
        ## 'ans' is the reverse permutation of 'oo'
        ans <- integer(length(oo))
        ans[oo] <- seq_len(length(oo))
        ans
    }
)

