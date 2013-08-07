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

### The 2 default methods below are implemented on top of compare().

setMethods("==", .BIN_COMP_OP_SIGNATURES,
    function(e1, e2) { compare(e1, e2) == 0L }
)

setMethods("<=", .BIN_COMP_OP_SIGNATURES,
    function(e1, e2) { compare(e1, e2) <= 0L }
)

### The 4 default methods below are implemented on top of == and <=.

setMethods("!=", .BIN_COMP_OP_SIGNATURES, function(e1, e2) { !(e1 == e2) })

setMethods(">=", .BIN_COMP_OP_SIGNATURES, function(e1, e2) { e2 <= e1 })

setMethods("<", .BIN_COMP_OP_SIGNATURES, function(e1, e2) { !(e2 <= e1) })

setMethods(">", .BIN_COMP_OP_SIGNATURES, function(e1, e2) { !(e1 <= e2) })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### unique()
###
### The default method below is implemented on top of duplicated().
###

### S3/S4 combo for unique.Vector
unique.Vector <- function(x, incomparables=FALSE, ...)
    extractROWS(x, !duplicated(x, incomparables=incomparables, ...))
setMethod("unique", "Vector", unique.Vector)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### %in%
###
### The default method below is implemented on top of match().
###

setMethods("%in%", .BIN_COMP_OP_SIGNATURES,
    function(x, table) { !is.na(match(x, table)) }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### findMatches() & countMatches()
###
### The default "findMatches" and "countMatches" methods below are implemented
### on top of match().
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
    table2 <- match(table, table, ...)
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

### Default "findMatches" method. Args in ... are passed down to match().
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

### Default "countMatches" method. Args in ... are passed down to match().
.countMatches.default <- function(x, table, ...)
{
    x_len <- length(x)
    table_len <- length(table)
    if (x_len <= table_len) {
        table2 <- match(table, x, ...)  # can contain NAs
        nbins <- x_len
        x2 <- match(x, x, ...)  # no NAs
    } else {
        table2 <- match(table, table, ...)  # no NAs
        nbins <- table_len + 1L
        x2 <- match(x, table, nomatch=nbins, ...)
    }
    tabulate(table2, nbins=nbins)[x2]
}

setMethod("countMatches", c("ANY", "ANY"), .countMatches.default)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### sort()
###
### The default method below is implemented on top of order().
###

### S3/S4 combo for sort.Vector
.sort.Vector <- function(x, decreasing=FALSE, na.last=NA)
    extractROWS(x, order(x, na.last=na.last, decreasing=decreasing))
sort.Vector <- function (x, decreasing=FALSE, ...)
    .sort.Vector(x, decreasing=decreasing, ...)
setMethod("sort", "Vector", sort.Vector)

