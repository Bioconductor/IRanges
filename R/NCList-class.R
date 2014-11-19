### =========================================================================
### NCList and NCLists objects
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### An S4 implementation of Nested Containment List (NCList).
###

setClass("NCList",
    contains="Ranges",
    representation(
        nclist="integer",
        ranges="IRanges"
    )
)

setMethod("ranges", "NCList", function(x, ...) x@ranges)
setMethod("length", "NCList", function(x) length(ranges(x)))
setMethod("names", "NCList", function(x) names(ranges(x)))
setMethod("start", "NCList", function(x, ...) start(ranges(x)))
setMethod("end", "NCList", function(x, ...) end(ranges(x)))
setMethod("width", "NCList", function(x) width(ranges(x)))

setAs("NCList", "IRanges", function(from) ranges(from))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### .shift_ranges_to_first_circle()
###
### TODO: Move to intra-range-methods.R, rename (e.g. shiftToFirstCircle()),
### make it a generic with methods for IRanges and IRangesList, export, and
### document.
###

.normarg_circle.length1 <- function(circle.length)
{
    msg <- "'circle.length' must be a single positive integer or NA"
    if (!isSingleNumberOrNA(circle.length))
        stop(msg)
    if (!is.integer(circle.length))
        circle.length <- as.integer(circle.length)
    if (!is.na(circle.length) && circle.length <= 0L)
        stop(msg)
    circle.length
}

.normarg_circle.length2 <- function(circle.length, x_len, what)
{
    msg <- c("'circle.length' must be an integer vector ",
             "with positive or NA values")
    if (!is.atomic(circle.length))
        stop(msg)
    if (!(is.numeric(circle.length) || all(is.na(circle.length))))
        stop(msg)
    if (!is.integer(circle.length))
        circle.length <- as.integer(circle.length)
    min_circle.length <- suppressWarnings(min(circle.length, na.rm=TRUE))
    if (is.finite(min_circle.length) && min_circle.length <= 0L)
        stop(msg)
    if (length(circle.length) == x_len)
        return(circle.length)
    if (length(circle.length) != 1L)
        stop("'circle.length' must have length 1 or length of ", what)
    rep.int(circle.length, x_len)
}

.shift_ranges_to_first_circle <- function(x, circle.length)
{
    circle.length <- .normarg_circle.length2(circle.length, length(x), "'x'")
    x_start0 <- start(x) - 1L  # 0-based start
    x_shift0 <- x_start0 %% circle.length - x_start0
    x_shift0[is.na(x_shift0)] <- 0L
    shift(x, x_shift0)
}

.shift_rglist_to_first_circle <- function(x, circle.length)
{
    circle.length <- .normarg_circle.length2(circle.length, length(x), "'x'")
    circle.length <- rep.int(circle.length, elementLengths(x))
    unlisted_x <- unlist(x, use.names=FALSE)
    unlisted_ans <- .shift_ranges_to_first_circle(unlisted_x, circle.length)
    relist(unlisted_ans, x)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### NCList constructor
###

### Returns an external pointer to the pre-NCList.
.preNCList <- function(x_start, x_end)
{
    ans <- .Call("preNCList_new", PACKAGE="IRanges")
    reg.finalizer(ans,
        function(e) .Call("preNCList_free", e, PACKAGE="IRanges")
    )
    .Call("preNCList_build", ans, x_start, x_end, PACKAGE="IRanges")
}

.nclist <- function(x_start, x_end)
{
    pnclist <- .preNCList(x_start, x_end)
    .Call("new_NCList_from_preNCList", pnclist, PACKAGE="IRanges")
}

### Usage:
###   x <- IRanges(c(11, 10, 13, 10, 14,  8, 10, 11),
###                c(15, 12, 18, 13, 14, 12, 15, 15))
###   subject <- NCList(x)
NCList <- function(x, circle.length=NA_integer_)
{
    if (!is(x, "Ranges"))
        stop("'x' must be a Ranges object")
    if (!is(x, "IRanges"))
        x <- as(x, "IRanges")
    x <- .shift_ranges_to_first_circle(x, circle.length)
    ans_nclist <- .nclist(start(x), end(x))
    new2("NCList", nclist=ans_nclist, ranges=x, check=FALSE)
}

setAs("Ranges", "NCList", function(from) NCList(from))

### NOT exported.
print_NCList <- function(x)
{
    if (!is(x, "NCList"))
        stop("'x' must be an NCList object")
    .Call("NCList_print", x@nclist, start(x@ranges), end(x@ranges),
                          PACKAGE="IRanges")
    invisible(NULL)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Representation of a list of NCList objects
###

setClass("NCLists",
    contains="RangesList",
    representation(
        nclists="list",
        rglist="CompressedIRangesList"
    ),
    prototype(
        elementType="NCList"
    )
)

setMethod("parallelSlotNames", "NCLists",
    function(x) c("nclists", "rglist", callNextMethod())
)

### TODO: Move rglist() generic from GenomicRanges to IRanges
#setMethod("rglist", "NCLists", function(x, ...) x@ranges)
setMethod("ranges", "NCLists", function(x, ...) x@rglist)
setMethod("length", "NCLists", function(x) length(ranges(x)))
setMethod("names", "NCLists", function(x) names(ranges(x)))
setMethod("start", "NCLists", function(x, ...) start(ranges(x)))
setMethod("end", "NCLists", function(x, ...) end(ranges(x)))
setMethod("width", "NCLists", function(x) width(ranges(x)))

setMethod("elementLengths", "NCLists", function(x) elementLengths(ranges(x)))
setMethod("getListElement", "NCLists",
    function (x, i, exact=TRUE)
    {
        i <- normalizeDoubleBracketSubscript(i, x, exact=exact,
                                             error.if.nomatch=TRUE)
        new2("NCList", nclist=x@nclists[[i]], ranges=x@rglist[[i]],
                       check=FALSE)
    }
)

setAs("NCLists", "CompressedIRangesList", function(from) ranges(from))
setAs("NCLists", "IRangesList", function(from) ranges(from))

### NCLists constructor.
NCLists <- function(x, circle.length=NA_integer_)
{
    if (!is(x, "RangesList"))
        stop("'x' must be a RangesList object")
    if (!is(x, "CompressedIRangesList"))
        x <- as(x, "CompressedIRangesList")
    x <- .shift_rglist_to_first_circle(x, circle.length)
    ans_nclists <- mapply(.nclist, start(x), end(x), SIMPLIFY=FALSE)
    new2("NCLists", nclists=ans_nclists, rglist=x, check=FALSE)
}

setAs("RangesList", "NCLists", function(from) NCLists(from))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### findOverlaps_NCList()
###

### NOT exported.
findOverlaps_NCList <- function(query, subject, min.score=1L,
                                type=c("any", "start", "end",
                                       "within", "extend", "equal"),
                                select=c("all", "first", "last", "arbitrary"),
                                circle.length=NA_integer_)
{
    if (!(is(query, "NCList") || is(subject, "NCList")))
        stop("'query' or 'subject' must be an NCList object")
    if (!isSingleNumber(min.score))
        stop("'min.score' must be a single integer")
    if (!is.integer(min.score))
        min.score <- as.integer(min.score)
    type <- match.arg(type)
    select <- match.arg(select)
    circle.length <- .normarg_circle.length1(circle.length)

    if (is(subject, "NCList")) {
        if (!is(query, "Ranges"))
            stop("'query' must be a Ranges object")
        ans <- .Call("NCList_find_overlaps",
                     start(query), end(query),
                     subject@nclist,
                     start(subject@ranges), end(subject@ranges),
                     min.score, type, select, circle.length,
                     PACKAGE="IRanges")
    } else {
        if (!is(subject, "Ranges"))
            stop("'subject' must be a Ranges object")
        if (type == "within")
            type <- "extend"
        else if (type == "extend")
            type <- "within"
        hits <- .Call("NCList_find_overlaps",
                      start(subject), end(subject),
                      query@nclist,
                      start(query@ranges), end(query@ranges),
                      min.score, type, "all", circle.length,
                      PACKAGE="IRanges")
        hits <- S4Vectors:::Hits_revmap(hits)
        ans <- selectHits(hits, select=select)
    }
    ans
}

### NOT exported.
NCList_which_to_preprocess <- function(query, subject)
{
    if (!(is(query, "Ranges") && is(subject, "Ranges")))
        stop("'query' and 'subject' must be Ranges objects")
    if (is(query, "NCList") || is(subject, "NCList"))
        stop("'query' or 'subject' is already an NCList object")

    ## Preprocessing the query instead of the subject is tempting when
    ## the query is shorter than the subject but then the Hits object
    ## returned by .Call entry point NCList_find_overlaps() needs to be
    ## reversed with S4Vectors:::Hits_revmap(). This operation requires
    ## sorting the this so its cost is in the order of NH * log(NH) where
    ## NH is the nb of hits. Because this extra cost cannot be known in
    ## advance and could possibly defeat the purpose of preprocessing the
    ## query instead of the subject, the empirical criteria for doing so
    ## is more conservative than just q_len <= s_len.
    q_len <- length(query)
    s_len <- length(subject)
    preprocess_q <- q_len == 0L || q_len * (log10(q_len))^2 <= s_len
    ifelse(preprocess_q, "query", "subject")
}

### NOT exported.
min_overlap_score <- function(maxgap=0L, minoverlap=1L)
{
    ## Check and normalize 'maxgap'.
    if (!isSingleNumber(maxgap))
        stop("'maxgap' must be a single integer")
    if (!is.integer(maxgap))
        maxgap <- as.integer(maxgap)

    ## Check and normalize 'minoverlap'.
    if (!isSingleNumber(minoverlap))
        stop("'minoverlap' must be a single integer")
    if (!is.integer(minoverlap))
        minoverlap <- as.integer(minoverlap)

    if (maxgap != 0L && minoverlap != 1L)
        stop("either 'maxgap' or 'minoverlap' can be specified but not both")
    if (maxgap < 0L)
        stop("'maxgap' cannot be negative")
    if (minoverlap < 0L)
        stop("'minoverlap' cannot be negative")
    minoverlap - maxgap
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### findOverlaps_NCLists()
###

### NOT exported.
### Return a list of Hits if 'select' is "all". Otherwise return a list of
### integer vectors. The returned list is parallel to 'query', and, if 'select'
### is not "all", also has the same shape as 'query'.
findOverlaps_NCLists <- function(query, subject, min.score=1L,
                                 type=c("any", "start", "end",
                                        "within", "extend", "equal"),
                                 select=c("all", "first", "last", "arbitrary"),
                                 circle.length=NA_integer_)
{
    if (!(is(query, "NCLists") || is(subject, "NCLists")))
        stop("'query' or 'subject' must be an NCLists object")
    if (!(is(query, "RangesList") && is(subject, "RangesList")))
        stop("'query' and 'subject' must be RangesList objects")
    query_len <- length(query)
    subject_len <- length(subject)
    if (!isSingleNumber(min.score))
        stop("'min.score' must be a single integer")
    if (!is.integer(min.score))
        min.score <- as.integer(min.score)
    type <- match.arg(type)
    select <- match.arg(select)
    circle.length <- .normarg_circle.length2(circle.length, query_len,
                                             "'query'")

    subject0 <- IRanges()
    if (is(subject, "NCLists"))
        subject0 <- NCList(subject0)
    lapply(seq_len(query_len),
           function(i) {
               subject_i <- if (i <= subject_len) subject[[i]] else subject0
               findOverlaps_NCList(query[[i]], subject_i,
                                   min.score=min.score,
                                   type=type, select=select,
                                   circle.length[[i]])
           })
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

if (FALSE) {  #     <<<--- begin testing findOverlaps_NCList() --->>>

### TEST 1:
### - NCList:       0.004 s / 6.9 s   / 408.3 Mb / 857m
### - IntervalTree: 0.004 s / 0.180 s /  65.0 Mb / 508m
library(RNAseqData.HNRNPC.bam.chr14)
library(GenomicAlignments)
bamfile <- RNAseqData.HNRNPC.bam.chr14_BAMFILES[1]
gal <- readGAlignmentsFromBam(bamfile)
reads <- ranges(gal)
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
ex <- exons(TxDb.Hsapiens.UCSC.hg19.knownGene)
seqlevels(ex, force=TRUE) <- "chr14"
ex14 <- ranges(ex)

system.time(subject <- NCList(ex14))
gc()
system.time(hits1 <- IRanges:::findOverlaps_NCList(reads, subject))
gc()

system.time(subject <- IntervalTree(ex14))
gc()
system.time(hits1b <- findOverlaps(reads, subject))
gc()

### TEST 2: Same as TEST 1 but switch query and subject.
### - NCList:       0.112 s /  0.06 s  / 64.5 Mb / 528m
### - IntervalTree: 0.138 s /  0.236 s / 63.8 Mb / 540m
system.time(subject <- NCList(reads))
gc()
system.time(hits2 <- IRanges:::findOverlaps_NCList(ex14, subject))
gc()

system.time(subject <- IntervalTree(reads))
gc()
system.time(hits2b <- findOverlaps(ex14, subject))
gc()

### TEST 3: NO duplicate ranges in subject!
### - NCList:       5.37 s /  2.33 s / 409.5 Mb / 1.079g (1.079g)
### - IntervalTree: 48.2 s / 13.25 s /  874.6 Mb / 2059m (3125m)
library(IRanges)
N <- 15000000L  # nb of ranges
W <- 180L       # range width
start <- 1L
end <- 150000000L
set.seed(777)
range_starts <- sample(end-W+1L, N, replace=FALSE)
range_widths <- rep.int(W, N)
x <- IRanges(start=range_starts, width=range_widths)
query <- successiveIRanges(rep(10000, 25010), from=-50000)

system.time(subject <- NCList(x))
gc()
system.time(hits3 <- IRanges:::findOverlaps_NCList(query, subject))
gc()

system.time(subject <- IntervalTree(x))
gc()
system.time(hits3b <- findOverlaps(query, subject))
gc()

### TEST 4: Same as TEST 3 but with duplicate ranges in subject.
### - NCList:       11.6 s /  4.2 s  / 1139.8 Mb / 1574m (2277m)
### - IntervalTree: 48.6 s / 13.24 s /  874.6 Mb / 2059m (3125m)
library(IRanges)
N <- 15000000L  # nb of ranges
W <- 180L       # range width
start <- 1L
end <- 150000000L
set.seed(777)
range_starts <- sample(end-W+1L, N, replace=TRUE)
range_widths <- rep.int(W, N)
x <- IRanges(start=range_starts, width=range_widths)
query <- successiveIRanges(rep(10000, 25010), from=-50000)

system.time(subject <- NCList(x))
gc()
system.time(hits4 <- IRanges:::findOverlaps_NCList(query, subject))
gc()

system.time(subject <- IntervalTree(x))
gc()
system.time(hits4b <- findOverlaps(query, subject))
gc()

### TEST 5:
### - NCList:       2.3 s / 3.4 s / 474.0 Mb / 708m (708m)
### - IntervalTree:   7 s / 5.9 s / 417.6 Mb / 640m (1221m)
library(IRanges)
N <- 5000000L  # nb of ranges
W <- 50000L    # max range width
start <- 1L
end <- 125000000L
set.seed(777)
range_starts <- sample(end-W+1L, N, replace=TRUE)
range_widths <- sample(W, N, replace=TRUE)
x <- IRanges(start=range_starts, width=range_widths)
query <- successiveIRanges(rep(10000, 12510), from=-50000)

system.time(subject <- NCList(x))
gc()
system.time(hits5 <- IRanges:::findOverlaps_NCList(query, subject))
gc()

library(digest)
stopifnot(identical(digest(hits5), "9cffe9ea1288cc60211afe044bd8e525"))

system.time(subject <- IntervalTree(x))
gc()
system.time(hits5b <- findOverlaps(query, subject))
gc()
stopifnot(identical(hits5, hits5b))

### TEST 6 (requires at least 16 Gb of RAM):
### - NCList:        30 s   / 35.5 s / 4222.0 Mb / 5494m (5891m)
### - IntervalTree: 106.4 s / 83.5 s / 3822.4 Mb / 4806m (11.2g)
library(IRanges)
N <- 50000000L  # nb of ranges
W <- 44000L     # max range width
start <- 1L
end <- 125000000L
set.seed(777)
range_starts <- sample(end-W+1L, N, replace=TRUE)
range_widths <- sample(W, N, replace=TRUE)
x <- IRanges(start=range_starts, width=range_widths)
query <- successiveIRanges(rep(10000, 12510), from=-50000)

system.time(subject <- NCList(x))
gc()
system.time(hits6 <- IRanges:::findOverlaps_NCList(query, subject))
gc()

system.time(subject <- IntervalTree(x))
gc()
system.time(hits6b <- findOverlaps(query, subject))
gc()

### TEST 7 (worst case scenario for NCList):
library(IRanges)
x <- IRanges(25000:1, 100001:125000)
query <- successiveIRanges(rep(100, 1020), from=-500)

system.time(subject <- NCList(x))
gc()
system.time(hits7 <- IRanges:::findOverlaps_NCList(query, subject))
gc()

system.time(subject <- IntervalTree(x))
gc()
system.time(hits7b <- findOverlaps(query, subject))
gc()

}

