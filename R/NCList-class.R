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

.normarg_circle.length2 <- function(circle.length, x_len, what,
                                    all.NAs.in.one=FALSE)
{
    msg <- c("'circle.length' must be an integer vector ",
             "with positive or NA values")
    if (!is.atomic(circle.length))
        stop(msg)
    if (!(length(circle.length) == 1L || length(circle.length) == x_len))
        stop("'circle.length' must have length 1 or length of ", what)
    all_NAs <- all(is.na(circle.length))
    if (all_NAs && all.NAs.in.one)
        return(NA_integer_)
    if (!(all_NAs || is.numeric(circle.length)))
        stop(msg)
    if (!is.integer(circle.length))
        circle.length <- as.integer(circle.length)
    if (!all_NAs && min(circle.length, na.rm=TRUE) <= 0L)
        stop(msg)
    if (length(circle.length) == x_len)
        return(circle.length)
    rep.int(circle.length, x_len)
}

.shift_ranges_to_first_circle <- function(x, circle.length)
{
    circle.length <- .normarg_circle.length2(circle.length, length(x), "'x'",
                                             all.NAs.in.one=TRUE)
    if (identical(circle.length, NA_integer_))
        return(x)
    x_start0 <- start(x) - 1L  # 0-based start
    x_shift0 <- x_start0 %% circle.length - x_start0
    x_shift0[is.na(x_shift0)] <- 0L
    shift(x, x_shift0)
}

.shift_rglist_to_first_circle <- function(x, circle.length)
{
    circle.length <- .normarg_circle.length2(circle.length, length(x), "'x'",
                                             all.NAs.in.one=TRUE)
    if (identical(circle.length, NA_integer_))
        return(x)
    circle.length <- rep.int(circle.length, elementLengths(x))
    unlisted_x <- unlist(x, use.names=FALSE)
    unlisted_ans <- .shift_ranges_to_first_circle(unlisted_x, circle.length)
    relist(unlisted_ans, x)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### NCList constructor
###

### Returns an external pointer to the pre-NCList.
.preNCList <- function(x_start, x_end, x_subset)
{
    ans <- .Call2("preNCList_new", PACKAGE="IRanges")
    reg.finalizer(ans,
        function(e) .Call("preNCList_free", e, PACKAGE="IRanges")
    )
    .Call2("preNCList_build", ans, x_start, x_end, x_subset, PACKAGE="IRanges")
}

.nclist <- function(x_start, x_end, x_subset=NULL)
{
    x_pnclist <- .preNCList(x_start, x_end, x_subset)
    .Call2("new_NCList_from_preNCList", x_pnclist, PACKAGE="IRanges")
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
    x_nclist <- .nclist(start(x), end(x))
    new2("NCList", nclist=x_nclist, ranges=x, check=FALSE)
}

setAs("Ranges", "NCList", function(from) NCList(from))

### NOT exported.
print_NCList <- function(x)
{
    if (!is(x, "NCList"))
        stop("'x' must be an NCList object")
    .Call2("NCList_print", x@nclist, start(x@ranges), end(x@ranges),
                           PACKAGE="IRanges")
    invisible(NULL)
}


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
    if (!(is(query, "Ranges") || is(subject, "Ranges")))
        stop("'query' and 'subject' must be Ranges objects")
    if (!isSingleNumber(min.score))
        stop("'min.score' must be a single integer")
    if (!is.integer(min.score))
        min.score <- as.integer(min.score)
    type <- match.arg(type)
    select <- match.arg(select)
    circle.length <- .normarg_circle.length1(circle.length)

    if (is(subject, "NCList")) {
        nclist <- subject@nclist
        nclist_is_q <- FALSE
        subject <- subject@ranges
    } else if (is(query, "NCList")) {
        nclist <- query@nclist
        nclist_is_q <- TRUE
        query <- query@ranges
    } else {
        nclist <- NULL
        nclist_is_q <- NA
    }
    .Call2("NCList_find_overlaps",
           start(query), end(query),
           start(subject), end(subject),
           nclist, nclist_is_q,
           min.score, type, select, circle.length,
           PACKAGE="IRanges")
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

.extract_groups_from_RangesList <- function(x)
{
    x_partitioning <- PartitioningByEnd(x)
    relist(as.integer(x_partitioning) - 1L, x_partitioning)
}

.nclists <- function(x, x_groups)
{
    x_start <- start(x)
    x_end <- end(x)
    lapply(x_groups, function(group) .nclist(x_start, x_end, x_subset=group))
}

### NCLists constructor.
NCLists <- function(x, circle.length=NA_integer_)
{
    if (!is(x, "RangesList"))
        stop("'x' must be a RangesList object")
    if (!is(x, "CompressedIRangesList"))
        x <- as(x, "CompressedIRangesList")
    x <- .shift_rglist_to_first_circle(x, circle.length)
    x_groups <- .extract_groups_from_RangesList(x)
    x_nclists <- .nclists(unlist(x, use.names=FALSE), x_groups)
    new2("NCLists", nclists=x_nclists, rglist=x, check=FALSE)
}

setAs("RangesList", "NCLists", function(from) NCLists(from))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### NCList_find_overlaps_in_groups()
###

### NOT exported. Workhorse behind findOverlaps_NCLists() below and behind
### GenomicRanges:::findOverlaps_GNCList().
NCList_find_overlaps_in_groups <- function(
                        q, q.space, q.groups,
                        s, s.space, s.groups,
                        nclists, nclist_is_q,
                        min.score, type, select, circle.length)
{
    if (!(is(q, "Ranges") || is(s, "Ranges")))
        stop("'q' and 's' must be Ranges object")
    .Call2("NCList_find_overlaps_in_groups",
           start(q), end(q), q.space, q.groups,
           start(s), end(s), s.space, s.groups,
           nclists, nclist_is_q,
           min.score, type, select, circle.length,
           PACKAGE="IRanges")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### findOverlaps_NCLists()
###

.split_and_remap_hits <- function(all_hits, query, subject)
{
    q_hits <- queryHits(all_hits)
    query_breakpoints <- end(PartitioningByEnd(query))
    h_skeleton <- PartitioningByEnd(findInterval(query_breakpoints, q_hits))

    ## Compute list element lengths and offsets for 'query'.
    query_partitioning <- PartitioningByEnd(query)
    query_eltlens <- width(query_partitioning)
    query_offsets <- start(query_partitioning) - 1L

    ## Compute list element lengths and offsets for 'subject'.
    subject_partitioning <- PartitioningByEnd(subject)
    subject_eltlens <- width(subject_partitioning)
    subject_offsets <- start(subject_partitioning) - 1L

    lapply(seq_along(h_skeleton),
           function(i) {
               hits <- all_hits[h_skeleton[[i]]]
               hits@queryHits <- hits@queryHits - query_offsets[[i]]
               hits@subjectHits <- hits@subjectHits - subject_offsets[[i]]
               hits@queryLength <- query_eltlens[[i]]
               hits@subjectLength <- subject_eltlens[[i]]
               hits
           })
}

### NOT exported.
### Return an ordinary list of:
###   (a) Hits objects if 'select' is "all". In that case the list has the
###       length of the shortest of 'query' or 'subject'.
###   (b) integer vectors if 'select' is not "all". In that case the list is
###       parallel to and has the same shape as 'query'.
findOverlaps_NCLists <- function(query, subject, min.score=1L,
                                 type=c("any", "start", "end",
                                        "within", "extend", "equal"),
                                 select=c("all", "first", "last", "arbitrary"),
                                 circle.length=NA_integer_)
{
    if (!(is(query, "RangesList") || is(subject, "RangesList")))
        stop("'query' and 'subject' must be RangesList objects")
    if (!isSingleNumber(min.score))
        stop("'min.score' must be a single integer")
    if (!is.integer(min.score))
        min.score <- as.integer(min.score)
    type <- match.arg(type)
    select <- match.arg(select)
    circle.length <- .normarg_circle.length2(circle.length,
                              max(length(query), length(subject)),
                              "longest of 'query' or 'subject'")
    if (is(subject, "NCLists")) {
        nclists <- subject@nclists
        nclist_is_q <- rep.int(FALSE, length(nclists))
        subject <- subject@rglist
    } else if (is(query, "NCLists")) {
        nclists <- query@nclists
        nclist_is_q <- rep.int(TRUE, length(nclists))
        query <- query@rglist
    } else {
        NG <- min(length(query), length(subject))
        nclists <- vector(mode="list", length=NG)
        nclist_is_q <- rep.int(NA, length(nclists))
    }

    if (!is(query, "CompressedIRangesList"))
        query <- as(query, "CompressedIRangesList")
    q <- unlist(query, use.names=FALSE)
    q_groups <- .extract_groups_from_RangesList(query)

    if (!is(subject, "CompressedIRangesList"))
        subject <- as(subject, "CompressedIRangesList")
    s <- unlist(subject, use.names=FALSE)
    s_groups <- .extract_groups_from_RangesList(subject)

    all_hits <- NCList_find_overlaps_in_groups(
                        q, NULL, q_groups,
                        s, NULL, s_groups,
                        nclists, nclist_is_q,
                        min.score, type, select, circle.length)
    .split_and_remap_hits(all_hits, query, subject)
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

