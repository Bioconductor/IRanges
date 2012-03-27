### =========================================================================
### encodeOverlaps()
### -------------------------------------------------------------------------
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### encodeOverlaps1() - A low-level utility.
###
###   > query <- IRanges(c(7, 15, 22), c(9, 19, 23))
###   > subject <- IRanges(c(1, 4, 15, 22), c(2, 9, 19, 25))
###   > encodeOverlaps1(query, subject, as.matrix=TRUE)
###        [,1] [,2] [,3] [,4]
###   [1,] "m"  "j"  "a"  "a" 
###   [2,] "m"  "m"  "g"  "a" 
###   [3,] "m"  "m"  "m"  "f" 
###   > encodeOverlaps1(query, subject)  # Type II encoding
###   $Loffset
###   [1] 1
###   
###   $Roffset
###   [1] 0
###   
###   $encoding
###   [1] "3:jmm:agm:aaf:"
###
encodeOverlaps1 <- function(query, subject,
                            query.space=NULL, subject.space=NULL,
                            Lquery.length=0L,
                            as.matrix=FALSE, as.raw=FALSE)
{
    if (!isSingleNumber(Lquery.length))
        stop("'Lquery.length' must be a single integer value")
    if (!is.integer(Lquery.length))
        Lquery.length <- as.integer(Lquery.length)
    if (!isTRUEorFALSE(as.matrix))
        stop("'as.matrix' must be TRUE or FALSE")
    if (!isTRUEorFALSE(as.raw))
        stop("'as.raw' must be TRUE or FALSE")
    .Call2("encode_overlaps1",
           start(query), width(query), query.space, Lquery.length,
           start(subject), width(subject), subject.space,
           as.matrix, as.raw,
           PACKAGE="IRanges")
}

### TODO: Put this in the (upcoming) man page for encodeOverlaps().
### A simple (but inefficient) implementation of the "findOverlaps" method for
### Ranges objects. Complexity and memory usage is M x N where M and N are the
### lengths of 'query' and 'subject', respectively.
findRangesOverlaps <- function(query, subject)
{
    ovenc <- encodeOverlaps1(query, subject, as.matrix=TRUE, as.raw=TRUE)
    offsets <- which(charToRaw("c") <= ovenc & ovenc <= charToRaw("k")) - 1L
    q_hits <- offsets %% nrow(ovenc) + 1L
    s_hits <- offsets %/% nrow(ovenc) + 1L
    cbind(queryHits=q_hits, subjectHits=s_hits)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### encodeOverlaps()
###

RangesList_encodeOverlaps <- function(query.starts, query.widths,
                                      subject.starts, subject.widths,
                                      query.spaces=NULL, subject.spaces=NULL,
                                      Lquery.lengths=NULL)
{
    C_ans <- .Call2("RangesList_encode_overlaps",
                    query.starts, query.widths, query.spaces, Lquery.lengths,
                    subject.starts, subject.widths, subject.spaces,
                    PACKAGE="IRanges")
    encoding <- factor(C_ans$encoding)
    new2("OverlapEncodings", Loffset=C_ans$Loffset, Roffset=C_ans$Roffset,
                             encoding=encoding, check=FALSE)
}

setGeneric("encodeOverlaps",
    function(query, subject, hits=NULL, ...) standardGeneric("encodeOverlaps")
)

setMethod("encodeOverlaps", c("ANY", "ANY", "Hits"),
    function(query, subject, hits=NULL, ...)
    {
        encodeOverlaps(query[queryHits(hits)], subject[subjectHits(hits)], ...)
    }
)

setMethod("encodeOverlaps", c("RangesList", "RangesList", "missing"),
    function(query, subject, hits=NULL, ...)
    {
        RangesList_encodeOverlaps(as.list(start(query)),
                                  as.list(width(query)),
                                  as.list(start(subject)),
                                  as.list(width(subject)))
    }
)

setMethod("encodeOverlaps", c("RangesList", "Ranges", "missing"),
    function(query, subject, hits=NULL, ...)
    {
        RangesList_encodeOverlaps(as.list(start(query)),
                                  as.list(width(query)),
                                  as.list(start(subject)),
                                  as.list(width(subject)))
    }
)

setMethod("encodeOverlaps", c("Ranges", "RangesList", "missing"),
    function(query, subject, hits=NULL, ...)
    {
        RangesList_encodeOverlaps(as.list(start(query)),
                                  as.list(width(query)),
                                  as.list(start(subject)),
                                  as.list(width(subject)))
    }
)

### Not sure we need to bother with methods that do 1-to-1 range overlap
### encodings. What would be the use cases?
###   > query <- IRanges(1:11, width=4)
###   > subject <- IRanges(6, 9)
###   > encodeOverlaps(query, subject)
###    [1] a b c c c g k k k l m
###   Levels: a b c d e f g h i j k l m X
###   > encodeOverlaps(IRanges(4:6, width=6), subject)
###   [1] d e h
###   Levels: a b c d e f g h i j k l m X
###   > encodeOverlaps(IRanges(6:8, width=2), subject)
###   [1] f i j
###   Levels: a b c d e f g h i j k l m X
setMethod("encodeOverlaps", c("Ranges", "Ranges", "missing"),
    function(query, subject,  hits=NULL, ...)
    {
        rangeComparisonCodeToLetter(compare(query, subject))
    }
)

