### =========================================================================
### Comparing and ordering the elements in one or more XRawList objects
### -------------------------------------------------------------------------
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### compare() and related methods.
###

setMethod("compare", c("XRawList", "XRawList"),
    function(x, y)
        .Call2("XRawList_compare", x, y, PACKAGE="IRanges")
)

setMethod("==", c("XRawList", "XRawList"),
    function(e1, e2) compare(e1, e2) == 0L
)

setMethod("<=", c("XRawList", "XRawList"),
    function(e1, e2) compare(e1, e2) <= 0L
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Order and rank of the elements in an XRawList object.
###

setMethod("is.unsorted", "XRawList",
    function(x, na.rm=FALSE, strictly=FALSE)
    {
        if (!identical(na.rm, FALSE))
            warning("\"is.unsorted\" method for XRawList objects ",
                    "ignores the 'na.rm' argument")
        if (!isTRUEorFALSE(strictly))
            stop("'strictly' must be TRUE or FALSE")
        .Call2("XRawList_is_unsorted", x, strictly, PACKAGE="IRanges")
    }
)

setMethod("order", "XRawList",
    function(..., na.last=TRUE, decreasing=FALSE)
    {
        if (!identical(na.last, TRUE))
            warning("\"order\" method for XRawList objects ",
                    "ignores the 'na.last' argument")
        if (!isTRUEorFALSE(decreasing))
            stop("'decreasing' must be TRUE or FALSE")
        ## All arguments in '...' are guaranteed to be XRawList objects.
        args <- list(...)
        if (length(args) == 1L) {
            x <- args[[1L]]
            return(.Call2("XRawList_order", x, decreasing, PACKAGE="IRanges"))
        }
        stop("\"order\" method for XRawList objects ",
             "only takes 1 XRawList object for now, sorry")
    }
)

setMethod("rank", "XRawList",
    function(x, na.last=TRUE,
             ties.method=c("average", "first", "random", "max", "min"))
    {
        if (!identical(na.last, TRUE))
            warning("\"rank\" method for XRawList objects ",
                    "ignores the 'na.last' argument")
        ties.method <- match.arg(ties.method)
        if (!(ties.method %in% c("first", "min")))
            stop("\"rank\" method for XRawList objects supports ",
                 "only 'ties.method=\"first\"' and 'ties.method=\"min\"'")
        .Call2("XRawList_rank", x, ties.method, PACKAGE="IRanges")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Matches between 2 XRawList objects, and self-matches within an XRawList
### object.
###

setMethod("match", c("XRawList", "XRawList"),
    function(x, table, nomatch=NA_integer_, incomparables=NULL)
    {
        if (!is.numeric(nomatch) || length(nomatch) != 1L)
            stop("'nomatch' must be a single integer value")
        if (!is.integer(nomatch))
            nomatch <- as.integer(nomatch)
        if (!is.null(incomparables))
            stop("\"match\" method for XRawList objects ",
                 "only accepts 'incomparables=NULL'")
        .Call2("XRawList_match_hash", x, table, nomatch, PACKAGE="IRanges")
    }
)

.selfmatchXRawList <- function(x)
{
    .Call2("XRawList_selfmatch_hash", x, PACKAGE="IRanges")
}

setMethod("duplicated", "XRawList",
    function(x, incomparables=FALSE)
    {
        if (!identical(incomparables, FALSE))
            stop("\"duplicated\" method for XRawList objects ",
                 "only accepts 'incomparables=FALSE'")
        sm <- .selfmatchXRawList(x)
        sm != seq_len(length(sm))
    }
)

