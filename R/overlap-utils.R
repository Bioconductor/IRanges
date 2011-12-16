### =========================================================================
### Utilities for generalized comparison of ranges and for overlap encoding
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### compare()
###

setGeneric("compare", function(x, y) standardGeneric("compare"))

### "Parallel" generalized comparison of 2 Ranges objects.
###   > x <- IRanges(1:11, width=4)
###   > y <- IRanges(6, 9)
###   > compare(x, y)
###    [1] -6 -5 -4 -4 -4  0  4  4  4  5  6
###   > compare(IRanges(4:6, width=6), y)
###   [1] -3 -2  1
###   > compare(IRanges(6:8, width=2), y)
###   [1] -1  2  3
###   > compare(x, y) < 0  # equivalent to x < y
###   > compare(x, y) == 0  # equivalent to x == y
###   > compare(x, y) > 0  # equivalent to x > y
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### encodeOverlaps()
###

setGeneric("encodeOverlaps", signature=c("query", "subject"),
    function(query, subject, sparse.output=TRUE, ...)
        standardGeneric("encodeOverlaps")
)

###   > query <- IRanges(c(7, 15, 22), c(9, 19, 23))
###   > subject <- IRanges(c(1, 4, 15, 22), c(2, 9, 19, 25))
###   > encodeOverlaps(query, subject, sparse.output=FALSE)
###   [1] "mjaa" "mmga" "mmmf"
###   > encodeOverlaps(query, subject)
###   [1] "2j:g:f"
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
    ocodes <- encodeOverlaps(query, subject, sparse.output=FALSE, as.raw=TRUE)
    offsets <- which(charToRaw("c") <= ocodes & ocodes <= charToRaw("k")) - 1L
    q_hits <- offsets %/% nrow(ocodes) + 1L
    s_hits <- offsets %% nrow(ocodes) + 1L
    cbind(query=q_hits, subject=s_hits)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### pencodeOverlaps()
###

### "Parallel" overlap encoding of 2 lists of Ranges objects.
###   > read1 <- IRanges(c(7, 15, 22), c(9, 19, 23))
###   > read2 <- IRanges(c(5, 15), c(9, 17))
###   > read3 <- IRanges(c(16, 22), c(19, 24))
###   > tx <- IRanges(c(1, 4, 15, 22), c(2, 9, 19, 25))
###   > query <- IRangesList(read1, read2, read3)
###   > subject <- IRangesList(transcript)
###   > ocodes <- pencodeOverlaps(as.list(start(query)), as.list(width(query)),
###                               as.list(start(tx)), as.list(width(tx)))
###   > ocodes
###   [1] "2j:g:f" "2j:f"   "3j:f"
### Reads compatible with this transcript:
###   > pattern <- "^[0-9]+([fgij]|(j|g)(:g)*:(g|f))$"
###   > grep(pattern, ocodes)
###   [1] 1 2 3
### No generic for now, just a simple wrapper to the .Call entry point.
pencodeOverlaps <- function(query.starts, query.widths,
                            subject.starts, subject.widths,
                            query.spaces=NULL, subject.spaces=NULL)
{
    .Call2("RangesList_pencode_overlaps",
           query.starts, query.widths, query.spaces,
           subject.starts, subject.widths, subject.spaces,
           PACKAGE="IRanges")
}

