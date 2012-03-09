### =========================================================================
### OverlapEncodings objects
### -------------------------------------------------------------------------
###

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### compare()
###
### TODO: This should probably go somewhere else e.g. in Ranges-comparison.R
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
### The OverlapEncodings class.
###
### The OverlapEncodings class is a container for storing a vector of OVM's.
### An OVM ("overlaps matrix") is an M x N matrix of 1-letter codes
### representing all the range-to-range overlaps between a query with M ranges
### and a subject with N ranges.
###

### Slots:
###   o Loffset ("left offset"): nb of cols on the left of the OVM that contain
###     only "m"'s.
###   o Roffset ("right offset"): nb of cols on the right of the OVM that
###     contain only "a"'s.
###   o encoding: linear sequence of symbols representing the trimmed OVM (i.e.
###     after removing Loffset cols on the left and Roffset cols on the right).
setClass("OverlapEncodings",
    contains="Vector",
    representation(
        Loffset="integer",    # no NAs, >= 0
        Roffset="integer",    # no NAs, >= 0
        encoding="factor"     # no NAs
    )
)

setGeneric("Loffset", function(x) standardGeneric("Loffset"))
setMethod("Loffset", "OverlapEncodings", function(x) x@Loffset)

setGeneric("Roffset", function(x) standardGeneric("Roffset"))
setMethod("Roffset", "OverlapEncodings", function(x) x@Roffset)

setGeneric("encoding", function(x) standardGeneric("encoding"))
setMethod("encoding", "OverlapEncodings", function(x) x@encoding)

setMethod("length", "OverlapEncodings", function(x) length(encoding(x)))

setMethod("as.data.frame", "OverlapEncodings",
    function(x, row.names=NULL, optional=FALSE, ...)
    {
        if (!(is.null(row.names) || is.character(row.names)))
            stop("'row.names' must be NULL or a character vector")
        data.frame(Loffset=Loffset(x),
                   Roffset=Roffset(x),
                   encoding=encoding(x),
                   row.names=row.names,
                   check.rows=TRUE,
                   check.names=FALSE,
                   stringsAsFactors=FALSE)
    }
)

setMethod("show", "OverlapEncodings",
    function(object)
    {
        lo <- length(object)
        cat(class(object), " of length ", lo, "\n", sep="")
        if (lo == 0L)
            return(NULL)
        if (lo < 20L) {
            showme <-
              as.data.frame(object,
                            row.names=paste("[", seq_len(lo), "]", sep=""))
        } else {
            sketch <- function(x)
              c(as.character(window(x, 1L, 9L)),
                "...",
                as.character(window(x, length(x)-8L, length(x))))
            showme <-
              data.frame(Loffset=sketch(Loffset(object)),
                         Roffset=sketch(Roffset(object)),
                         encoding=sketch(encoding(object)),
                         row.names=c(paste("[", 1:9, "]", sep=""), "...",
                                     paste("[", (lo-8L):lo, "]", sep="")),
                         check.rows=TRUE,
                         check.names=FALSE,
                         stringsAsFactors=FALSE)
        }
        show(showme)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### encodeOverlaps()
###

### Low-level utility:
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
### TODO: Do we really need this? Same could be achieved with
### 'encodeOverlaps(IRangesList(query), IRangesList(subject))' except that
### with encodeOverlaps1() we can specify 'query.space' and 'subject.space'
### and use the 'as.matrix' and 'as.raw' flags to control the format of the
### output.
### Also the C code behind encodeOverlaps1() is at the heart of all the
### "encodeOverlaps" methods. Playing with encodeOverlaps1() with different
### inputs and combinations of the 'as.matrix' and 'as.raw' flags is
### educative and allows testing all parts of this C code.
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

setGeneric("encodeOverlaps", signature=c("query", "subject"),
    function(query, subject) standardGeneric("encodeOverlaps")
)

### "Parallel" overlap encoding between 2 RangesList objects.
### Between many reads and one transcript.
###   > read1 <- IRanges(c(7, 15, 22), c(9, 19, 23))
###   > read2 <- IRanges(c(5, 15), c(9, 17))
###   > read3 <- IRanges(c(16, 22), c(19, 24))
###   > query <- IRangesList(read1, read2, read3)
###   > tx <- IRanges(c(1, 4, 15, 22), c(2, 9, 19, 25))
###   > subject <- IRangesList(tx)
###   > ovenc <- encodeOverlaps(query, subject)
###   > ovenc
###   OverlapEncodings of length 3
###       Loffset Roffset       encoding
###   [1]       1       0 3:jmm:agm:aaf:
###   [2]       1       1       2:jm:af:
###   [3]       2       0       2:jm:af:
### Reads compatible with transcript 'tx':
###   ## Regex to use for reads with 1 range (no gaps):
###   > pattern1 <- ":[fgij]:"
###   ## Regex to use for reads with 2 ranges (1 gap):
###   > pattern2 <- ":[jg].:.[gf]:"
###   ## Regex to use for reads with 3 ranges (2 gaps):
###   > pattern3 <- ":[jg]..:.g.:..[gf]:"
###   ## Regex to use for reads with up to 2 gaps:
###   > pattern123 <- ":([fgij]|[jg].:.[gf]|[jg]..:.g.:..[gf]):"
###   > grep(pattern123, encoding(ovenc))
###   [1] 1 2 3
### All the reads are compatible with this transcript!
setMethod("encodeOverlaps", c("RangesList", "RangesList"),
    function(query, subject)
    {
        RangesList_encodeOverlaps(as.list(start(query)),
                                  as.list(width(query)),
                                  as.list(start(subject)),
                                  as.list(width(subject)))
    }
)

setMethod("encodeOverlaps", c("RangesList", "Ranges"),
    function(query, subject)
    {
        RangesList_encodeOverlaps(as.list(start(query)),
                                  as.list(width(query)),
                                  as.list(start(subject)),
                                  as.list(width(subject)))
    }
)

setMethod("encodeOverlaps", c("Ranges", "RangesList"),
    function(query, subject)
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
###   Levels: a b c d e f g h i j k l m
###   > encodeOverlaps(IRanges(4:6, width=6), subject)
###   [1] d e h
###   Levels: a b c d e f g h i j k l m
###   > encodeOverlaps(IRanges(6:8, width=2), subject)
###   [1] f i j
###   Levels: a b c d e f g h i j k l m
setMethod("encodeOverlaps", c("Ranges", "Ranges"),
    function(query, subject)
    {
        ### TODO: Maybe add an extra arg to compare() to let the user choose
        ### the type of output i.e. numeric or 1-letter codes.
        codes <- compare(query, subject)
        numTo1Letter <- function(codes)
            safeExplode(rawToChar(as.raw(as.integer(charToRaw("g")) + codes)))
        factor(numTo1Letter(codes), levels=numTo1Letter(-6:6))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Some convenience wrappers.
###

encodeOverlapsFromHits <- function(x, query, subject)
{
    if (!is(x, "Hits"))
        stop("'x' must be a Hits object")
    encodeOverlaps(query[queryHits(x)], subject[subjectHits(x)])
}

