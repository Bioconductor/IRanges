### =========================================================================
### Comparing and ordering ranges
### -------------------------------------------------------------------------
###


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

`%in%.warning.msg` <- function(classname)
{
    msg <- c("Starting with BioC 2.12, the behavior of %%in%% ",
             "on %s objects\n  has changed to use *equality* instead ",
             "of *overlap* for comparing\n  elements between %s objects ",
             "'x' and 'table'. Now 'x[i]' and \n  'table[j]' are ",
             "considered to match when they are equal (i.e. 'x[i] ==\n  ",
             "table[j]'), instead of when they overlap. ",
             "This new behavior is consistent\n  with base::`%%in%%`(). ",
             "If you need the old behavior, please use:\n\n",
             "    query %%over%% subject\n\n  ",
             "If you need the new behavior, you can use suppressWarnings()\n  ",
             "to suppress this warning.")
    fmt <- paste0(msg, collapse="")
    sprintf(fmt, classname, classname)
}

### TODO: Defunct 'match.if.overlap' arg in BioC 2.14.
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
            msg <- c("  In the near future (starting with BioC 2.14), ",
                     "match() on Ranges objects\n  won't support ",
                     "the 'match.if.overlap' argument anymore. Please use\n\n",
                     "    findOverlaps(x, table, select=\"first\")\n\n",
                     "  instead of\n\n",
                     "    match(x, table, match.if.overlap=TRUE)")
            .Deprecated(msg=msg)
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
### TODO: Defunct 'match.if.overlap' arg in BioC 2.14.
setMethod("selfmatch", "Ranges",
    function(x, method=c("auto", "quick", "hash"), match.if.overlap=FALSE)
        selfmatchIntegerPairs(start(x), width(x), method=method)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### duplicated()
###
### duplicated() would normally work out-of-the-box on Ranges objects thanks
### to the method for Vector objects. However the method for AtomicList
### vector is in the way and breaks this grand scheme. So we need to override
### it with a specific method for Ranges objects that calls the method for
### Vector objects.
###

### S3/S4 combo for duplicated.Ranges
duplicated.Ranges <- duplicated.Vector
setMethod("duplicated", "Ranges", duplicated.Ranges)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### %in%
###
### %in% will work out-of-the-box on Ranges objects thanks to the method
### for Vector objects.
### The only reason for overriding the method for Vector objects is to issue
### the warning.
### TODO: Remove this method in BioC 2.14 when the 'match.if.overlap' arg
### of match() is defunct.
###

setMethod("%in%", c("Ranges", "Ranges"),
    function(x, table)
    {
        warning(`%in%.warning.msg`("Ranges"))
        !is.na(match(x, table))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### findMatches() & countMatches()
###
### findMatches() & countMatches() will work out-of-the-box on Ranges objects
### thanks to the methods for Vector objects.
### The only reason for defining the 2 methods below is to prevent the
### warnings that otherwise would be issued when the user calls findMatches()
### or countMatches() on Ranges objects.
### TODO: Remove these methods in BioC 2.14 when the 'match.if.overlap' arg
### of match() is defunct.
###

setMethod("findMatches", c("Ranges", "Ranges"),
    function(x, table, select=c("all", "first", "last"), ...)
    {
        select <- match.arg(select)
        if (select != "all")
            stop("'select' is not supported yet. Note that you can use ",
                 "match() if you want to do 'select=\"first\"'. Otherwise ",
                 "you're welcome to request this on the Bioconductor ",
                 "mailing list.")
        .findAllMatchesInSmallTable(x, table, match.if.overlap=FALSE, ...)
    }
)

setMethod("countMatches", c("Ranges", "Ranges"),
    function(x, table, ...)
        .countMatches.default(x, table, match.if.overlap=FALSE, ...)
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

