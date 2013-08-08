### =========================================================================
### Comparing and ordering vector-like objects
### -------------------------------------------------------------------------
###


### Method signatures for binary comparison operators.
.BIN_COMP_OP_SIGNATURES <- list(
    c("Vector", "Vector"),
    c("Vector", "ANY"),
    c("ANY", "Vector")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Element-wise (aka "parallel") comparison of 2 Vector objects.
###

setGeneric("compare", function(x, y) standardGeneric("compare"))

### The methods below are implemented on top of compare().

setMethods("==", .BIN_COMP_OP_SIGNATURES,
    function(e1, e2) { compare(e1, e2) == 0L }
)

setMethods("<=", .BIN_COMP_OP_SIGNATURES,
    function(e1, e2) { compare(e1, e2) <= 0L }
)

### The methods below are implemented on top of == and <=.

setMethods("!=", .BIN_COMP_OP_SIGNATURES, function(e1, e2) { !(e1 == e2) })

setMethods(">=", .BIN_COMP_OP_SIGNATURES, function(e1, e2) { e2 <= e1 })

setMethods("<", .BIN_COMP_OP_SIGNATURES, function(e1, e2) { !(e2 <= e1) })

setMethods(">", .BIN_COMP_OP_SIGNATURES, function(e1, e2) { !(e1 <= e2) })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### selfmatch()
###
### The default "selfmatch" method below is implemented on top of match().
###

setGeneric("selfmatch",
    function(x, ...) standardGeneric("selfmatch")
)

### Default "selfmatch" method. Args in ... are propagated to match().
setMethod("selfmatch", "ANY", function(x, ...) match(x, x, ...))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### duplicated() & unique()
###
### The "duplicated" method below is implemented on top of selfmatch().
### The "unique" method below is implemented on top of duplicated().
###

### S3/S4 combo for duplicated.Vector
duplicated.Vector <- function(x, incomparables=FALSE, ...)
{
    if (!identical(incomparables, FALSE)) 
        stop("the \"duplicated\" method for Vector objects ", 
             "only accepts 'incomparables=FALSE'")
    args <- list(...)
    if ("fromLast" %in% names(args)) {
        fromLast <- args$fromLast
        if (!isTRUEorFALSE(fromLast)) 
            stop("'fromLast' must be TRUE or FALSE")
        args$fromLast <- NULL
        if (fromLast)
            x <- rev(x)
    } else {
        fromLast <- FALSE
    }
    xx <- do.call(selfmatch, c(list(x), args))
    ans <- xx != seq_along(xx)
    if (fromLast)
        ans <- rev(ans)
    ans
}
setMethod("duplicated", "Vector", duplicated.Vector)

### S3/S4 combo for unique.Vector
unique.Vector <- function(x, incomparables=FALSE, ...)
    extractROWS(x, !duplicated(x, incomparables=incomparables, ...))
setMethod("unique", "Vector", unique.Vector)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### %in%
###
### The method below is implemented on top of match().
###

setMethods("%in%", .BIN_COMP_OP_SIGNATURES,
    function(x, table) { !is.na(match(x, table)) }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### findMatches() & countMatches()
###
### The default "findMatches" and "countMatches" methods below are
### implemented on top of match().
###

setGeneric("findMatches", signature=c("x", "table"),
    function(x, table, select=c("all", "first", "last"), ...)
        standardGeneric("findMatches")
)

### Equivalent to 'countQueryHits(findMatches(x, table))' but the default
### "countMatches" method below has a more efficient implementation.
setGeneric("countMatches", signature=c("x", "table"),
    function(x, table, ...)
        standardGeneric("countMatches")
)

### Problem: using transpose=TRUE generates an invalid Hits object (hits are
### not ordered):
###   > IRanges:::.findAllMatchesInSmallTable(1:6, c(7:5, 4:5), transpose=TRUE)
###   Hits of length 4
###   queryLength: 5
###   subjectLength: 6
###     queryHits subjectHits 
###      <integer>   <integer> 
###    1         4           4 
###    2         3           5 
###    3         5           5 
###    4         2           6
### and the cost of ordering them would probably defeat the purpose of the
### "put the smallest object on the right" optimization trick.
.findAllMatchesInSmallTable <- function(x, table, ..., transpose=FALSE)
{
    x2 <- match(x, table, ...)
    table2 <- selfmatch(table, ...)
    table_low2high <- makeLow2highFromHigh2low(table2)
    hits_per_x <- table_low2high[x2]
    x_hits <- rep.int(seq_along(hits_per_x),
                      elementLengths(hits_per_x))
    if (length(x_hits) == 0L) {
        table_hits <- integer(0)
    } else {
        table_hits <- unlist(hits_per_x, use.names=FALSE)
    }
    if (transpose) {
        new2("Hits", queryHits=table_hits, subjectHits=x_hits,
                     queryLength=length(table), subjectLength=length(x),
                     check=FALSE)
    } else {
        new2("Hits", queryHits=x_hits, subjectHits=table_hits,
                     queryLength=length(x), subjectLength=length(table),
                     check=FALSE)
    }
}

### Default "findMatches" method. Args in ... are propagated to match() and
### selfmatch().
setMethod("findMatches", c("ANY", "ANY"),
    function(x, table, select=c("all", "first", "last"), ...)
    {
        select <- match.arg(select)
        if (select != "all")
            stop("'select' is not supported yet. Note that you can use ",
                 "match() if you want to do 'select=\"first\"'. Otherwise ",
                 "you're welcome to request this on the Bioconductor ",
                 "mailing list.")
        ## "put the smallest object on the right" optimization trick
        #if (length(x) < length(table))
        #    return(.findAllMatchesInSmallTable(table, x, ..., transpose=TRUE))
        .findAllMatchesInSmallTable(x, table, ...)
    }
)

### Default "countMatches" method. Args in ... are propagated to match() and
### selfmatch().
.countMatches.default <- function(x, table, ...)
{
    x_len <- length(x)
    table_len <- length(table)
    if (x_len <= table_len) {
        table2 <- match(table, x, ...)  # can contain NAs
        nbins <- x_len
        x2 <- selfmatch(x, ...)  # no NAs
    } else {
        table2 <- selfmatch(table, ...)  # no NAs
        nbins <- table_len + 1L
        x2 <- match(x, table, nomatch=nbins, ...)
    }
    tabulate(table2, nbins=nbins)[x2]
}

setMethod("countMatches", c("ANY", "ANY"), .countMatches.default)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### sort()
###
### The method below is implemented on top of order().
###

### S3/S4 combo for sort.Vector
.sort.Vector <- function(x, decreasing=FALSE, na.last=NA)
    extractROWS(x, order(x, na.last=na.last, decreasing=decreasing))
sort.Vector <- function (x, decreasing=FALSE, ...)
    .sort.Vector(x, decreasing=decreasing, ...)
setMethod("sort", "Vector", sort.Vector)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### table()
###
### The method below is implemented on top of selfmatch(), order(), and
### as.character().
###

### This is a copy/paste of the list.names() function locally defined inside
### base::table().
.list.names <- function(...) {
    deparse.level <- 1
    l <- as.list(substitute(list(...)))[-1L]
    nm <- names(l)
    fixup <- if (is.null(nm))
        seq_along(l)
    else nm == ""
    dep <- vapply(l[fixup], function(x) switch(deparse.level +
        1, "", if (is.symbol(x)) as.character(x) else "",
        deparse(x, nlines = 1)[1L]), "")
    if (is.null(nm))
        dep
    else {
        nm[fixup] <- dep
        nm
    }
}

.compute_table <- function(x)
{
    xx <- selfmatch(x)
    t <- tabulate(xx, nbins=length(xx))
    keep_idx <- which(t != 0L)
    x2 <- x[keep_idx]
    t2 <- t[keep_idx]
    oo <- order(x2)
    x2 <- x2[oo]
    t2 <- t2[oo]
    ans <- array(t2)
    dimnames(ans) <- list(as.character(x2))
    ans
}

setMethod("table", "Vector",
    function(...)
    {
        args <- list(...)
        if (length(args) > 1L)
            stop("the \"table\" method for Vector objects currently ",
                 "only supports one argument")
        x <- args[[1L]]

        ## Compute the table as an array.
        ans <- .compute_table(x)

        ## Some cosmetic adjustments.
        names(dimnames(ans)) <- .list.names(...)
        class(ans) <- "table"
        ans
    }
)

