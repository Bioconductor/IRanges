### =========================================================================
### encodeOverlaps()
### -------------------------------------------------------------------------
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### encodeOverlaps1() - A low-level utility.
###
###   > query <- IRanges(start=c(7, 15, 22), end=c(9, 19, 23))
###   > subject <- IRanges(start=c(1, 4, 15, 22, 1, 30, 25),
###                        end=c(2, 9, 19, 25, 10, 38, 25))
###   > encodeOverlaps1(query, subject, as.matrix=TRUE)
###       [,1] [,2] [,3] [,4] [,5] [,6] [,7]
###   [1,] "m"  "j"  "a"  "a"  "i"  "a"  "a" 
###   [2,] "m"  "m"  "g"  "a"  "m"  "a"  "a" 
###   [3,] "m"  "m"  "m"  "f"  "m"  "a"  "a" 
###   > encodeOverlaps1(query, subject)
###   $Loffset
###   [1] 1
###   
###   $Roffset
###   [1] 2
###   
###   $encoding
###   [1] "3:jmm:agm:aaf:imm:"
###
###   > query.space <- c(0, 1, 0)
###   > encodeOverlaps1(query, subject, query.space=query.space)$encoding
###   [1] "3:mXm:jXm:aXm:aXf:iXm:aXa:aXa:"
###   > query.space <- rep(-1, length(query))
###   > subject.space <- rep(-1, length(subject))
###   > encodeOverlaps1(rev(query), rev(subject),
###                     query.space=query.space, subject.space=subject.space)
###   $Loffset
###   [1] 2
###
###   $Roffset
###   [1] 1
###
###   $encoding
###   [1] "3:aai:jmm:agm:aaf:"
###
###   > encodeOverlaps1(query, subject, query.break=2)$encoding
###   [1] "2--1:jm--m:ag--m:aa--f:im--m:"
###   > encodeOverlaps1(rev(query), rev(subject),
###                     query.space=query.space, subject.space=subject.space,
###                     query.break=1)$encoding
###   [1] "1--2:a--ai:j--mm:a--gm:a--af:"

### 'query.space' must be either an integer vector of the same length as
### 'query', or NULL. If NULL, then it's interpreted as
### 'integer(length(query))' i.e. all the ranges in 'query' are considered to
### be on space 0.
encodeOverlaps1 <- function(query, subject,
                            query.space=NULL, subject.space=NULL,
                            query.break=0L, flip.query=FALSE,
                            as.matrix=FALSE, as.raw=FALSE)
{
    if (!is(query, "Ranges"))
        stop("'query' must be a Ranges object")
    if (!is(subject, "Ranges"))
        stop("'subject' must be a Ranges object")
    if (is.numeric(query.space) && !is.integer(query.space))
        query.space <- as.integer(query.space)
    if (is.numeric(subject.space) && !is.integer(subject.space))
        subject.space <- as.integer(subject.space)
    if (!isSingleNumber(query.break))
        stop("'query.break' must be a single integer value")
    if (!is.integer(query.break))
        query.break <- as.integer(query.break)
    if (!isTRUEorFALSE(flip.query))
        stop("'flip.query' must be TRUE or FALSE")
    if (!isTRUEorFALSE(as.matrix))
        stop("'as.matrix' must be TRUE or FALSE")
    if (!isTRUEorFALSE(as.raw))
        stop("'as.raw' must be TRUE or FALSE")
    .Call2("encode_overlaps1",
           start(query), width(query), query.space,
           query.break, flip.query,
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
### The RangesList_encodeOverlaps() helper.
###
### This is the power horse behind all the "encodeOverlaps" methods.
###

.RangesList_encodeOverlaps <- function(query.starts, query.widths,
                                       query.spaces, query.breaks,
                                       subject.starts, subject.widths,
                                       subject.spaces)
{
    .Call2("RangesList_encode_overlaps",
           query.starts, query.widths, query.spaces, query.breaks,
           subject.starts, subject.widths, subject.spaces,
           PACKAGE="IRanges")
}

.Hits_encode_overlaps <- function(query.starts, query.widths,
                                  query.spaces, query.breaks,
                                  subject.starts, subject.widths,
                                  subject.spaces,
                                  hits, flip.query)
{
    if (queryLength(hits) != length(query.starts) ||
        subjectLength(hits) != length(subject.starts))
        stop("'hits' is not compatible with 'query' and 'subject'")
    .Call2("Hits_encode_overlaps",
           query.starts, query.widths, query.spaces, query.breaks,
           subject.starts, subject.widths, subject.spaces,
           queryHits(hits), subjectHits(hits), flip.query,
           PACKAGE="IRanges")
}

RangesList_encodeOverlaps <- function(query.starts, query.widths,
                                      subject.starts, subject.widths,
                                      hits, flip.query=NULL,
                                      query.spaces=NULL, subject.spaces=NULL,
                                      query.breaks=NULL)
{
    if (is.null(hits)) {
        C_ans <- .RangesList_encodeOverlaps(query.starts, query.widths,
                                            query.spaces, query.breaks,
                                            subject.starts, subject.widths,
                                            subject.spaces)
        flip.query <- logical(length(encoding))
    } else {
        if (!is(hits, "Hits"))
            stop("'hits' must be a Hits object")
        if (is.null(flip.query)) {
            flip.query <- logical(length(hits))
        } else {
            if (!is.logical(flip.query))
                stop("'flip.query' must be a logical vector")
            if (length(flip.query) != length(hits))
                stop("'flip.query' must have the same length as 'hits'")
        }
        C_ans <- .Hits_encode_overlaps(query.starts, query.widths,
                                       query.spaces, query.breaks,
                                       subject.starts, subject.widths,
                                       subject.spaces,
                                       hits, flip.query)
    }
    encoding <- factor(C_ans$encoding)
    new2("OverlapEncodings", Loffset=C_ans$Loffset, Roffset=C_ans$Roffset,
                             encoding=encoding, flippedQuery=flip.query,
                             check=FALSE)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### encodeOverlaps()
###

setGeneric("encodeOverlaps", signature=c("query", "subject"),
    function(query, subject, hits=NULL, ...) standardGeneric("encodeOverlaps")
)

setMethod("encodeOverlaps", c("RangesList", "RangesList"),
    function(query, subject, hits=NULL, ...)
    {
        RangesList_encodeOverlaps(as.list(start(query)),
                                  as.list(width(query)),
                                  as.list(start(subject)),
                                  as.list(width(subject)),
                                  hits)
    }
)

setMethod("encodeOverlaps", c("RangesList", "Ranges"),
    function(query, subject, hits=NULL, ...)
    {
        RangesList_encodeOverlaps(as.list(start(query)),
                                  as.list(width(query)),
                                  as.list(start(subject)),
                                  as.list(width(subject)),
                                  hits)
    }
)

setMethod("encodeOverlaps", c("Ranges", "RangesList"),
    function(query, subject, hits=NULL, ...)
    {
        RangesList_encodeOverlaps(as.list(start(query)),
                                  as.list(width(query)),
                                  as.list(start(subject)),
                                  as.list(width(subject)),
                                  hits)
    }
)

