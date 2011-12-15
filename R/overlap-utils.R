###

setGeneric("compare", function(x, y) standardGeneric("compare"))

### > x <- IRanges(1:11, width=4)
### > y <- IRanges(6, 9)
### > compare(x, y)
###  [1] -6 -5 -4 -4 -4  0  4  4  4  5  6
### > compare(IRanges(4:6, width=6), y)
### [1] -3 -2  1
### > compare(IRanges(6:8, width=2), y)
### [1] -1  2  3
### > compare(x, y) < 0  # equivalent to x < y
### > compare(x, y) == 0  # equivalent to x == y
### > compare(x, y) > 0  # equivalent to x > y
### TODO: Seems like using compare() to implement "==", "!=", "<=", ">=",
### "<" and ">" methods for Ranges objects would make them slightly faster
### (between 1.5x and 2.5x) and also slightly more memory efficient.
setMethod("compare", c("Ranges", "Ranges"),
    function(x, y)
    {
        .Call2("Ranges_compare",
               start(x), width(x), start(y), width(y),
               PACKAGE="IRanges")
    }
)

setGeneric("encodeOverlaps", signature=c("query", "subject"),
    function(query, subject, sparse.output=TRUE, ...)
        standardGeneric("encodeOverlaps")
)

### > query <- IRanges(c(7, 15, 22), c(9, 19, 23))
### > subject <- IRanges(c(1, 4, 15, 22), c(2, 9, 19, 25))
### > encodeOverlaps(query, subject, sparse.output=FALSE)
### [1] "mjaa" "mmga" "mmmf"
### > encodeOverlaps(query, subject)
### [1] "2:j<g<f"
setMethod("encodeOverlaps", c("Ranges", "Ranges"),
    function(query, subject, sparse.output=TRUE,
             query.space=NULL, subject.space=NULL, as.raw=FALSE)
    {
        if (!isTRUEorFALSE(sparse.output))
            stop("'sparse.output' must be TRUE or FALSE")
        if (!isTRUEorFALSE(as.raw))
            stop("'as.raw' must be TRUE or FALSE")
        .Call2("Ranges_encode_overlaps",
               start(query), width(query), query.space,
               start(subject), width(subject), subject.space,
               sparse.output, as.raw,
               PACKAGE="IRanges")
    }
)

### TODO: Put this in the (upcoming) man page for encodeOverlaps().
### A simple (but inefficient) implementation of the "findOverlaps" method for
### Ranges objects. Complexity and memory usage is M x N where M and N are the
### lengths of 'query' and 'subject', respectively.
findRangesOverlaps <- function(query, subject)
{
    ## WARNING: When using sparse.output=FALSE and as.raw=TRUE, the returned
    ## raw matrix is transposed!
    codes <- encodeOverlaps(query, subject, sparse.output=FALSE, as.raw=TRUE)
    offsets <- which(charToRaw("c") <= codes & codes <= charToRaw("k")) - 1L
    q_hits <- offsets %/% nrow(codes) + 1L
    s_hits <- offsets %% nrow(codes) + 1L
    cbind(query=q_hits, subject=s_hits)
}

