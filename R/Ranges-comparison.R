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
### This "compare" method returns one of the 13 predefined codes (>= -6 and
### <= 6) described in the man page. The signs of those codes reflect this
### order.
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
### We only need to implement "==" and "<=" methods. The other comparison
### binary operators (!=, >=, <, >) will then work out-of-the-box on
### Ranges objects thanks to the methods for Vector objects.
###

setMethod("==", signature(e1="Ranges", e2="Ranges"),
    function(e1, e2) { compare(e1, e2) == 0L }
)

setMethod("<=", signature(e1="Ranges", e2="Ranges"),
    function(e1, e2) { compare(e1, e2) <= 0L }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### duplicated()
###
### unique() will work out-of-the-box on a Ranges object thanks to the
### method for Vector objects.
###

.duplicated.Ranges <- function(x, incomparables=FALSE, fromLast=FALSE,
                               method=c("auto", "quick", "hash"))
{
    if (!identical(incomparables, FALSE))
        stop("\"duplicated\" method for Ranges objects ",
             "only accepts 'incomparables=FALSE'")
    duplicatedIntegerPairs(start(x), width(x),
                           fromLast=fromLast, method=method)
}
### S3/S4 combo for duplicated.Ranges
duplicated.Ranges <- function(x, incomparables=FALSE, ...)
    .duplicated.Ranges(x, incomparables=incomparables, ...)
setMethod("duplicated", "Ranges", .duplicated.Ranges)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### match()
###
### %in% will work out-of-the-box on Ranges objects thanks to the method
### for Vector objects.
###

match.if.overlap.warning.msg <- function(classname)
{
    msg <- c("Starting with BioC 2.12, the default behavior of match() ",
             "on %s\n  objects has changed to use *equality* instead ",
             "of *overlap* for comparing\n  elements between %s objects ",
             "'x' and 'table'. Now 'x[i]' and\n  'table[j]' are ",
             "considered to match when they are equal (i.e. 'x[i] ==\n  ",
             "table[j]'), instead of when they overlap. ",
             "This new behavior is consistent\n  with base::match(). ",
             "If you need the old behavior, you can either do:\n\n",
             "    findOverlaps(x, table, select=\"first\")  # recommended\n\n",
             "  or, alternatively, call match() with 'match.if.overlap=TRUE' ",
             "(explicitly\n  provide this argument to suppress this warning).")
    fmt <- paste0(msg, collapse="")
    sprintf(fmt, classname, classname)
}

`%in%.warning.msg` <- function(classname)
{
    msg <- c("Starting with BioC 2.12, the behavior of %%in%% ",
             "on %s objects\n  has changed to use *equality* instead ",
             "of *overlap* for comparing\n  elements between %s objects ",
             "'x' and 'table'. Now 'x[i]' and \n  'table[j]' are ",
             "considered to match when they are equal (i.e. 'x[i] ==\n  ",
             "table[j]'), instead of when they overlap. ",
             "This new behavior is consistent\n  with base::`%%in%%`(). ",
             "If you need the old behavior, you can do:\n\n",
             "    !is.na(findOverlaps(x, table, select=\"arbitrary\"))\n\n  ",
             "You can use suppressWarnings() to suppress this warning.")
    fmt <- paste0(msg, collapse="")
    sprintf(fmt, classname, classname)
}

### Unfortunately, the early version of this method was doing overlaps, not
### equality. We temporarily add the 'match.if.overlap' argument so the old
### behavior is still available.
### TODO: Deprecate 'match.if.overlap' arg in BioC 2.13.
### TODO: Remove 'match.if.overlap' arg in BioC 2.14.
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
        if (missing(match.if.overlap))
            warning(match.if.overlap.warning.msg("Ranges"))
        if (!isTRUEorFALSE(match.if.overlap))
            stop("'match.if.overlap' must be TRUE or FALSE")
        if (match.if.overlap) {
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

### The only reason for overriding the method for Vector objects is to issue
### the warning.
### TODO: Remove this method in BioC 2.14 when the 'match.if.overlap' arg of
### match() is gone.
setMethod("%in%", c("Ranges", "Ranges"),
    function(x, table)
    {
        warning(`%in%.warning.msg`("Ranges"))
        !is.na(match(x, table, match.if.overlap=FALSE))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### order() and related methods.
###
### The "order" and "rank" methods for Ranges objects are consistent with the
### order implied by compare().
### sort() will work out-of-the-box on a Ranges object thanks to the method
### for Vector objects.
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

